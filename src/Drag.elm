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


type Drag
    = NoDrag
    | Drag
        { dragIdx : Int
        , xyDelta : XYDelta
        , dragElementOffset : XY
        }
    | DragOver
        { dragIdx : Int
        , xyDelta : XYDelta
        , dragElementOffset : XY
        , dropIdx : Int
        }


initial : Drag
initial =
    NoDrag


dragElementAndXY drag =
    case drag of
        NoDrag ->
            Nothing

        Drag { xyDelta, dragElementOffset } ->
            Just { xyDelta = xyDelta, dragElementOffset = dragElementOffset }

        DragOver { xyDelta, dragElementOffset } ->
            Just { xyDelta = xyDelta, dragElementOffset = dragElementOffset }


type Msg
    = GlobalMouseMove XY
    | GlobalMouseUp
    | MouseDownOnDraggable Int String XY
    | MouseOverDroppable Int
    | GotDragElement Int XY Element
    | GotDomElementError Dom.Error


subscriptions : Drag -> Sub Msg
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


xyMovedTo : XY -> Drag -> Drag
xyMovedTo xy model =
    let
        setCurrentXYIn state =
            { state | xyDelta = XYDelta.moveTo xy state.xyDelta }
    in
    case model of
        NoDrag ->
            model

        Drag state ->
            setCurrentXYIn state |> Drag

        DragOver state ->
            setCurrentXYIn state |> DragOver


dragEvents : (Msg -> msg) -> Int -> String -> Drag -> List (H.Attribute msg)
dragEvents tagger idx domId drag =
    case drag of
        NoDrag ->
            [ E.preventDefaultOn "mousedown"
                (XY.pageXYDecoder
                    |> JD.map (MouseDownOnDraggable idx domId >> tagger >> pd)
                )
            ]

        Drag _ ->
            []

        DragOver _ ->
            []


dropEvents : (Msg -> msg) -> Int -> Drag -> List (H.Attribute msg)
dropEvents tagger idx model =
    let
        events =
            [ E.onMouseOver (MouseOverDroppable idx |> tagger) ]
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


update : (Msg -> msg) -> Msg -> Drag -> ( Drag, Cmd msg )
update toMsg message model =
    let
        ( newModel, cmd ) =
            updateHelp message model
    in
    ( newModel, cmd |> Cmd.map toMsg )


updateHelp : Msg -> Drag -> ( Drag, Cmd Msg )
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

        MouseOverDroppable idx ->
            ( case model of
                NoDrag ->
                    model

                Drag { dragIdx, xyDelta, dragElementOffset } ->
                    DragOver
                        { dragIdx = dragIdx
                        , xyDelta = xyDelta
                        , dragElementOffset = dragElementOffset
                        , dropIdx = idx
                        }

                DragOver state ->
                    DragOver
                        { state
                            | dropIdx = idx
                        }
            , Cmd.none
            )

        GotDragElement dragIdx xy element ->
            ( Drag
                { dragIdx = dragIdx
                , xyDelta = XYDelta.init (XY.add xy (XY.subtract element.element element.viewport))
                , dragElementOffset = XY.subtract element.element element.viewport
                }
            , Cmd.none
            )

        GotDomElementError (Dom.NotFound _) ->
            ( NoDrag, Cmd.none )


ghostStyles : Drag -> Css.Style
ghostStyles =
    dragElementAndXY
        >> Maybe.map
            (\{ dragElementOffset, xyDelta } ->
                let
                    { x, y } =
                        XY.add (XYDelta.diff xyDelta)
                            dragElementOffset
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
