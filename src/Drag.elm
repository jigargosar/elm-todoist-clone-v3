module Drag exposing
    ( Drag
    , Msg
    , dragEvents
    , dropEvents
    , ghostStyles
    , initial
    , subscriptions
    , update
    )

import Basics.More exposing (flip)
import Browser.Dom as Dom exposing (Element)
import Browser.Events as BE
import Css
import Html.Styled as H
import Html.Styled.Events as E
import Json.Decode as JD
import Styles
import Task
import XY exposing (XY)
import XYDelta exposing (XYDelta)


type Drag a b
    = NoDrag
    | Drag
        { drag : a
        , mouseMoveDelta : XYDelta
        , dragElementOffset : XY
        }
    | DragOver
        { drag : a
        , mouseMoveDelta : XYDelta
        , dragElementOffset : XY
        , dragOver : b
        }


initial : Drag a b
initial =
    NoDrag


dragElementAndXY drag =
    case drag of
        NoDrag ->
            Nothing

        Drag { mouseMoveDelta, dragElementOffset } ->
            Just { mouseMoveDelta = mouseMoveDelta, dragElementOffset = dragElementOffset }

        DragOver { mouseMoveDelta, dragElementOffset } ->
            Just { mouseMoveDelta = mouseMoveDelta, dragElementOffset = dragElementOffset }


type Msg a b
    = GlobalMouseMove XY
    | GlobalMouseUp
    | MouseDownOnDraggable a String XY
    | MouseOverDroppable b
    | GotDragElement a XY Element
    | GotDomElementError Dom.Error


subscriptions : Drag a b -> Sub (Msg a b)
subscriptions drag =
    let
        subs =
            Sub.batch
                [ BE.onMouseMove (JD.map GlobalMouseMove XY.pageXYDecoder)
                , BE.onMouseUp (JD.succeed GlobalMouseUp)
                ]
    in
    case drag of
        NoDrag ->
            Sub.none

        Drag _ ->
            subs

        DragOver _ ->
            subs


xyMovedTo : XY -> Drag a b -> Drag a b
xyMovedTo xy model =
    let
        setCurrentXYIn state =
            { state | mouseMoveDelta = XYDelta.moveTo xy state.mouseMoveDelta }
    in
    case model of
        NoDrag ->
            model

        Drag state ->
            setCurrentXYIn state |> Drag

        DragOver state ->
            setCurrentXYIn state |> DragOver


dragEvents : (Msg a b -> msg) -> a -> String -> Drag a b -> List (H.Attribute msg)
dragEvents tagger a domId drag =
    case drag of
        NoDrag ->
            [ E.preventDefaultOn "mousedown"
                (XY.pageXYDecoder
                    |> JD.map (MouseDownOnDraggable a domId >> tagger >> pd)
                )
            ]

        Drag _ ->
            []

        DragOver _ ->
            []


dropEvents : (Msg a b -> msg) -> b -> Drag a b -> List (H.Attribute msg)
dropEvents tagger a model =
    let
        events =
            [ E.onMouseOver (MouseOverDroppable a |> tagger) ]
    in
    case model of
        NoDrag ->
            []

        Drag _ ->
            events

        DragOver _ ->
            events


pd =
    flip Tuple.pair False


update : (Msg a b -> msg) -> Msg a b -> Drag a b -> ( Drag a b, Cmd msg )
update toMsg message model =
    let
        ( newModel, cmd ) =
            updateHelp message model
    in
    ( newModel, cmd |> Cmd.map toMsg )


updateHelp : Msg a b -> Drag a b -> ( Drag a b, Cmd (Msg a b) )
updateHelp message model =
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
    case message of
        GlobalMouseMove xy ->
            ( xyMovedTo xy model, Cmd.none )

        GlobalMouseUp ->
            ( NoDrag, Cmd.none )

        MouseDownOnDraggable dragIdx dragId xy ->
            ( model
            , getElement dragId (GotDragElement dragIdx xy)
            )

        MouseOverDroppable b ->
            ( case model of
                NoDrag ->
                    model

                Drag { drag, mouseMoveDelta, dragElementOffset } ->
                    DragOver
                        { drag = drag
                        , mouseMoveDelta = mouseMoveDelta
                        , dragElementOffset = dragElementOffset
                        , dragOver = b
                        }

                DragOver state ->
                    DragOver { state | dragOver = b }
            , Cmd.none
            )

        GotDragElement a xy element ->
            ( Drag
                { drag = a
                , mouseMoveDelta = XYDelta.init (XY.add xy (XY.subtract element.element element.viewport))
                , dragElementOffset = XY.subtract element.element element.viewport
                }
            , Cmd.none
            )

        GotDomElementError (Dom.NotFound _) ->
            ( NoDrag, Cmd.none )


ghostStyles : Drag a b -> Css.Style
ghostStyles =
    dragElementAndXY
        >> Maybe.map
            (\{ dragElementOffset, mouseMoveDelta } ->
                let
                    { x, y } =
                        XY.add (XYDelta.diff mouseMoveDelta) dragElementOffset
                in
                [ Styles.absolute
                , Styles.top_0
                , Styles.left_0
                , Css.transform (Css.translate2 (Css.px 0) (Css.px y))
                , Css.pointerEvents Css.none
                ]
            )
        >> Maybe.withDefault []
        >> Css.batch
