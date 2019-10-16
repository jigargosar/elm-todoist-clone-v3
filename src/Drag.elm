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
                                onSuccess element
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
            getElement model.dropId GotDropElement

        DraggingOver _ ->
            Cmd.none


type Msg
    = GlobalMouseMove XY
    | GlobalMouseUp
    | MouseDownOnDragZone XY String
    | MouseOverDropZone String
    | GotDragElement Element
    | GotDropElement Element
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
