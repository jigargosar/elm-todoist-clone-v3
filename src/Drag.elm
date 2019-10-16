module Drag exposing (..)

import Browser.Dom as Dom exposing (Element)
import Browser.Events as BE
import Json.Decode as JD
import Task


type alias XY =
    { x : Float, y : Float }


type Drag
    = NoDrag
    | DragPending
        { dragId : String
        , startXY : XY
        , currentXY : XY
        }
    | Drag
        { dragId : String
        , startXY : XY
        , currentXY : XY
        , dragElement : Dom.Element
        }
    | DragOverPending
        { dragId : String
        , startXY : XY
        , currentXY : XY
        , dragElement : Dom.Element
        , dropId : String
        }
    | DragOver
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
        NoDrag ->
            Cmd.none

        DragPending model ->
            getElement model.dragId GotDragElement

        Drag _ ->
            Cmd.none

        DragOverPending model ->
            getElement model.dropId GotDroppableElement

        DragOver _ ->
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
        NoDrag ->
            Sub.none

        DragPending _ ->
            mouseSubscriptions

        Drag _ ->
            mouseSubscriptions

        DragOverPending _ ->
            mouseSubscriptions

        DragOver _ ->
            mouseSubscriptions


setCurrentXY : XY -> Drag -> Drag
setCurrentXY xy model =
    let
        setCurrentXYIn : { a | currentXY : b } -> { a | currentXY : b }
        setCurrentXYIn state =
            { state | currentXY = xy }
    in
    case model of
        NoDrag ->
            model

        DragPending state ->
            setCurrentXYIn state |> DragPending

        Drag state ->
            setCurrentXYIn state |> Drag

        DragOverPending state ->
            setCurrentXYIn state |> DragOverPending

        DragOver state ->
            setCurrentXYIn state |> DragOver


update message model =
    case message of
        GlobalMouseMove xy ->
            setCurrentXY xy model

        GlobalMouseUp ->
            NoDrag

        MouseDownOnDraggable dragId xy ->
            DragPending { dragId = dragId, startXY = xy, currentXY = xy }

        MouseOverDroppable domId ->
            case model of
                NoDrag ->
                    Debug.todo "MouseOverDropZone, NotDragging"

                DragPending _ ->
                    Debug.todo "MouseOverDropZone, DragStartPending"

                Drag { dragId, startXY, currentXY, dragElement } ->
                    DragOverPending
                        { dragId = dragId
                        , startXY = startXY
                        , currentXY = currentXY
                        , dragElement = dragElement
                        , dropId = domId
                        }

                DragOverPending state ->
                    { state | dropId = domId } |> DragOverPending

                DragOver { dragId, startXY, currentXY, dragElement, dropId } ->
                    if domId /= dropId then
                        DragOverPending
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
                DragPending { dragId, startXY, currentXY } ->
                    if dragId /= domId then
                        Debug.todo "Invalid State, GotDragElement, DragStartPending"

                    else
                        Drag
                            { dragId = dragId
                            , startXY = startXY
                            , currentXY = currentXY
                            , dragElement = element
                            }

                _ ->
                    Debug.todo <| "Invalid State: GotDragElement" ++ Debug.toString model

        GotDroppableElement domId element ->
            case model of
                DragOverPending { dragId, startXY, currentXY, dragElement, dropId } ->
                    if dropId /= domId then
                        Debug.todo "Invalid State, GotDropElement, DraggingOverPending"

                    else
                        DragOver
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
