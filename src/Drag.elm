module Drag exposing (Drag, Msg, XY, dragDomIdInfo, dragEvents, dragIdxInfo, dropEvents, initial, pageXYDecoder, subscriptions, update)

import Basics.More exposing (flip)
import Browser.Dom as Dom exposing (Element)
import Browser.Events as BE
import Html.Styled as H
import Html.Styled.Events as E
import Json.Decode as JD
import Task


type alias XY =
    { x : Float, y : Float }


type Drag
    = NoDrag
    | DragPending
        { dragId : String
        , dragIdx : Int
        , startXY : XY
        , currentXY : XY
        }
    | Drag
        { dragId : String
        , dragIdx : Int
        , startXY : XY
        , currentXY : XY
        , dragElement : Dom.Element
        }
    | DragOverPending
        { dragId : String
        , dragIdx : Int
        , startXY : XY
        , currentXY : XY
        , dragElement : Dom.Element
        , dropId : String
        , dropIdx : Int
        }
    | DragOver
        { dragId : String
        , dragIdx : Int
        , startXY : XY
        , currentXY : XY
        , dragElement : Dom.Element
        , dropId : String
        , dropIdx : Int
        , dropElement : Element
        }


initial : Drag
initial =
    NoDrag


dragDomIdInfo : Drag -> Maybe { dragId : String, dropId : String }
dragDomIdInfo model =
    case model of
        NoDrag ->
            Nothing

        DragPending { dragId } ->
            Just { dragId = dragId, dropId = dragId }

        Drag { dragId } ->
            Just { dragId = dragId, dropId = dragId }

        DragOverPending { dragId, dropId } ->
            Just { dragId = dragId, dropId = dropId }

        DragOver { dragId, dropId } ->
            Just { dragId = dragId, dropId = dropId }


dragIdxInfo : Drag -> Maybe { dragIdx : Int, dropIdx : Int }
dragIdxInfo model =
    case model of
        NoDrag ->
            Nothing

        DragPending { dragIdx } ->
            Just { dragIdx = dragIdx, dropIdx = dragIdx }

        Drag { dragIdx } ->
            Just { dragIdx = dragIdx, dropIdx = dragIdx }

        DragOverPending { dragIdx, dropIdx } ->
            Just { dragIdx = dragIdx, dropIdx = dropIdx }

        DragOver { dragIdx, dropIdx } ->
            Just { dragIdx = dragIdx, dropIdx = dropIdx }


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
            getElement model.dropId GotDropElement

        DragOver _ ->
            Cmd.none


type Msg
    = GlobalMouseMove XY
    | GlobalMouseUp
    | MouseDownOnDraggable Int String XY
    | MouseOverDroppable Int String
    | GotDragElement String Element
    | GotDropElement String Element
    | GotDomElementError Dom.Error


pageXYDecoder : JD.Decoder XY
pageXYDecoder =
    JD.map2 XY
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)


subscriptions : Drag -> Sub Msg
subscriptions drag =
    let
        getMouseUpOrMove =
            Sub.batch
                [ BE.onMouseMove (JD.map GlobalMouseMove pageXYDecoder)
                , getMouseUp
                ]

        getMouseUp =
            BE.onMouseUp (JD.succeed GlobalMouseUp)
    in
    case drag of
        NoDrag ->
            Sub.none

        DragPending _ ->
            getMouseUp

        Drag _ ->
            getMouseUpOrMove

        DragOverPending _ ->
            getMouseUp

        DragOver _ ->
            getMouseUpOrMove


setCurrentXY : XY -> Drag -> Drag
setCurrentXY xy model =
    let
        setCurrentXYIn : { a | currentXY : XY } -> { a | currentXY : XY }
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


dragStart : Int -> String -> XY -> Msg
dragStart idx domId xy =
    MouseDownOnDraggable idx domId xy


dragEvents : (Msg -> msg) -> Int -> String -> Drag -> List (H.Attribute msg)
dragEvents tagger idx domId drag =
    case drag of
        NoDrag ->
            [ E.preventDefaultOn "mousedown" (pageXYDecoder |> JD.map (dragStart idx domId >> tagger >> pd)) ]

        DragPending _ ->
            []

        Drag _ ->
            []

        DragOverPending _ ->
            []

        DragOver _ ->
            []


dropEvents : (Msg -> msg) -> Int -> String -> Drag -> List (H.Attribute msg)
dropEvents tagger idx domId model =
    let
        events =
            [ E.onMouseOver (MouseOverDroppable idx domId |> tagger) ]
    in
    case model of
        NoDrag ->
            []

        DragPending _ ->
            []

        Drag _ ->
            events

        DragOverPending _ ->
            events

        DragOver _ ->
            events


pd =
    flip Tuple.pair False


update : (Msg -> msg) -> Msg -> Drag -> ( Drag, Cmd msg )
update toMsg message model =
    let
        newModel =
            updateModel message model
    in
    ( newModel, commands newModel |> Cmd.map toMsg )


updateModel : Msg -> Drag -> Drag
updateModel message model =
    case message of
        GlobalMouseMove xy ->
            setCurrentXY xy model

        GlobalMouseUp ->
            NoDrag

        MouseDownOnDraggable dragIdx dragId xy ->
            DragPending { dragId = dragId, dragIdx = dragIdx, startXY = xy, currentXY = xy }

        MouseOverDroppable idx domId ->
            case model of
                NoDrag ->
                    Debug.todo "MouseOverDropZone, NotDragging"

                DragPending _ ->
                    Debug.todo "MouseOverDropZone, DragStartPending"

                Drag { dragId, dragIdx, startXY, currentXY, dragElement } ->
                    DragOverPending
                        { dragId = dragId
                        , dragIdx = dragIdx
                        , startXY = startXY
                        , currentXY = currentXY
                        , dragElement = dragElement
                        , dropId = domId
                        , dropIdx = idx
                        }

                DragOverPending state ->
                    { state | dropId = domId } |> DragOverPending

                DragOver { dragId, dragIdx, startXY, currentXY, dragElement, dropId, dropIdx } ->
                    if domId /= dropId then
                        DragOverPending
                            { dragId = dragId
                            , dragIdx = dragIdx
                            , startXY = startXY
                            , currentXY = currentXY
                            , dragElement = dragElement
                            , dropId = domId
                            , dropIdx = dropIdx
                            }

                    else
                        model

        GotDragElement domId element ->
            case model of
                DragPending { dragId, dragIdx, startXY, currentXY } ->
                    if dragId /= domId then
                        Debug.todo "Invalid State, GotDragElement, DragStartPending"

                    else
                        Drag
                            { dragId = dragId
                            , dragIdx = dragIdx
                            , startXY = startXY
                            , currentXY = currentXY
                            , dragElement = element
                            }

                _ ->
                    Debug.todo <| "Invalid State: GotDragElement" ++ Debug.toString model

        GotDropElement domId element ->
            case model of
                DragOverPending { dragId, dragIdx, startXY, currentXY, dragElement, dropId, dropIdx } ->
                    if dropId /= domId then
                        model

                    else
                        DragOver
                            { dragId = dragId
                            , dragIdx = dragIdx
                            , startXY = startXY
                            , currentXY = currentXY
                            , dragElement = dragElement
                            , dropId = domId
                            , dropIdx = dropIdx
                            , dropElement = element
                            }

                DragOver { dragId, dragIdx, startXY, currentXY, dragElement, dropId, dropIdx, dropElement } ->
                    if dropId /= domId then
                        model

                    else
                        DragOver
                            { dragId = dragId
                            , dragIdx = dragIdx
                            , startXY = startXY
                            , currentXY = currentXY
                            , dragElement = dragElement
                            , dropId = domId
                            , dropIdx = dropIdx
                            , dropElement = element
                            }

                NoDrag ->
                    model

                _ ->
                    Debug.todo <| "Invalid State: GotDropElement" ++ Debug.toString model

        GotDomElementError (Dom.NotFound domIdNF) ->
            Debug.todo <| "GotDomElementError: " ++ domIdNF
