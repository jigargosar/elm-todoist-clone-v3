module Drag exposing (..)

import Browser.Dom as Dom exposing (Element)
import Browser.Events as BE
import Json.Decode as JD
import Task


type alias XY =
    { x : Float, y : Float }


type Drag
    = NotDragging
    | DragStartPending
        { dragId : String
        , startXY : XY
        , currentXY : XY
        }
    | Dragging
        { dragId : String
        , startXY : XY
        , currentXY : XY
        , dragElement : Dom.Element
        }
    | DraggingOverPending
        { dragId : String
        , startXY : XY
        , currentXY : XY
        , dragElement : Dom.Element
        , dropId : String
        }
    | DraggingOver
        { dragId : String
        , startXY : XY
        , currentXY : XY
        , dragElement : Dom.Element
        , dropId : String
        , dropElement : Element
        }


commands : Drag -> Cmd Msg
commands drag =
    let
        getElement domId onSuccess =
            Dom.getElement domId
                |> Task.attempt
                    (\res ->
                        case res of
                            Err domError ->
                                GotDomElementError domError

                            Ok element ->
                                onSuccess domId element
                    )
    in
    case drag of
        NotDragging ->
            Cmd.none

        DragStartPending model ->
            getElement model.dragId GotDragElement

        Dragging _ ->
            Cmd.none

        DraggingOverPending model ->
            getElement model.dropId GotDroppableElement

        DraggingOver _ ->
            Cmd.none


type Msg
    = GlobalMouseMove XY
    | GlobalMouseUp
    | MouseDownOnDraggable String XY
    | MouseOverDroppable String
    | GotDragElement String Element
    | GotDroppableElement String Element
    | GotDomElementError Dom.Error


pageXYDecoder : JD.Decoder XY
pageXYDecoder =
    JD.map2 XY
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)


subscriptions : Drag -> Sub Msg
subscriptions drag =
    let
        mouseSubscriptions =
            Sub.batch
                [ BE.onMouseMove (JD.map GlobalMouseMove pageXYDecoder)
                , BE.onMouseUp (JD.succeed GlobalMouseUp)
                ]
    in
    case drag of
        NotDragging ->
            Sub.none

        DragStartPending _ ->
            mouseSubscriptions

        Dragging _ ->
            mouseSubscriptions

        DraggingOverPending _ ->
            mouseSubscriptions

        DraggingOver _ ->
            mouseSubscriptions


setCurrentXY : XY -> Drag -> Drag
setCurrentXY xy model =
    let
        setCurrentXYIn : { a | currentXY : b } -> { a | currentXY : b }
        setCurrentXYIn state =
            { state | currentXY = xy }
    in
    case model of
        NotDragging ->
            model

        DragStartPending state ->
            setCurrentXYIn state |> DragStartPending

        Dragging state ->
            setCurrentXYIn state |> Dragging

        DraggingOverPending state ->
            setCurrentXYIn state |> DraggingOverPending

        DraggingOver state ->
            setCurrentXYIn state |> DraggingOver


update message model =
    case message of
        GlobalMouseMove xy ->
            setCurrentXY xy model

        GlobalMouseUp ->
            NotDragging

        MouseDownOnDraggable dragId xy ->
            DragStartPending { dragId = dragId, startXY = xy, currentXY = xy }

        MouseOverDroppable domId ->
            case model of
                NotDragging ->
                    Debug.todo "MouseOverDropZone, NotDragging"

                DragStartPending _ ->
                    Debug.todo "MouseOverDropZone, DragStartPending"

                Dragging { dragId, startXY, currentXY, dragElement } ->
                    DraggingOverPending
                        { dragId = dragId
                        , startXY = startXY
                        , currentXY = currentXY
                        , dragElement = dragElement
                        , dropId = domId
                        }

                DraggingOverPending state ->
                    { state | dropId = domId } |> DraggingOverPending

                DraggingOver { dragId, startXY, currentXY, dragElement, dropId } ->
                    if domId /= dropId then
                        DraggingOverPending
                            { dragId = dragId
                            , startXY = startXY
                            , currentXY = currentXY
                            , dragElement = dragElement
                            , dropId = dropId
                            }

                    else
                        model

        GotDragElement domId element ->
            case model of
                DragStartPending { dragId, startXY, currentXY } ->
                    if dragId /= domId then
                        Debug.todo "Invalid State, GotDragElement, DragStartPending"

                    else
                        Dragging
                            { dragId = dragId
                            , startXY = startXY
                            , currentXY = currentXY
                            , dragElement = element
                            }

                _ ->
                    Debug.todo <| "Invalid State: GotDragElement" ++ Debug.toString model

        GotDroppableElement domId element ->
            case model of
                DraggingOverPending { dragId, startXY, currentXY, dragElement, dropId } ->
                    if dropId /= domId then
                        Debug.todo "Invalid State, GotDropElement, DraggingOverPending"

                    else
                        DraggingOver
                            { dragId = dragId
                            , startXY = startXY
                            , currentXY = currentXY
                            , dragElement = dragElement
                            , dropId = dropId
                            , dropElement = element
                            }

                _ ->
                    Debug.todo <| "Invalid State: GotDropElement" ++ Debug.toString model

        GotDomElementError (Dom.NotFound domIdNF) ->
            Debug.todo <| "GotDomElementError: " ++ domIdNF
