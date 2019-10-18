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


dragElementAndXY model =
    case model of
        NoDrag ->
            Nothing

        Drag { drag, mouseMoveDelta, dragElementOffset } ->
            Just { drag = drag, mouseMoveDelta = mouseMoveDelta, dragElementOffset = dragElementOffset }

        DragOver { drag, mouseMoveDelta, dragElementOffset } ->
            Just { drag = drag, mouseMoveDelta = mouseMoveDelta, dragElementOffset = dragElementOffset }


type Msg a b
    = GlobalMouseMove XY
    | GlobalMouseUp
    | MouseDownOnDraggable a String XY
    | MouseOverDroppable b
    | GotDragElement a XY Element
    | GotDomElementError Dom.Error


subscriptions : (Msg a b -> msg) -> Drag a b -> Sub msg
subscriptions toMsg drag =
    let
        subs =
            Sub.batch
                [ BE.onMouseMove (JD.map GlobalMouseMove XY.pageXYDecoder)
                , BE.onMouseUp (JD.succeed GlobalMouseUp)
                ]
                |> Sub.map toMsg
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


type alias Config a b =
    { canAccept : a -> b -> Bool }


update : (Msg a b -> msg) -> Config a b -> Msg a b -> Drag a b -> ( Drag a b, Cmd msg )
update toMsg config message model =
    let
        ( newModel, cmd ) =
            updateHelp config message model
    in
    ( newModel, cmd |> Cmd.map toMsg )


updateHelp : Config a b -> Msg a b -> Drag a b -> ( Drag a b, Cmd (Msg a b) )
updateHelp config message model =
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
                    if config.canAccept drag b then
                        DragOver
                            { drag = drag
                            , mouseMoveDelta = mouseMoveDelta
                            , dragElementOffset = dragElementOffset
                            , dragOver = b
                            }

                    else
                        model

                DragOver state ->
                    if config.canAccept state.drag b then
                        DragOver { state | dragOver = b }

                    else
                        model
            , Cmd.none
            )

        GotDragElement a xy element ->
            ( Drag
                { drag = a
                , mouseMoveDelta = XYDelta.init xy
                , dragElementOffset = XY.subtract element.element element.viewport
                }
            , Cmd.none
            )

        GotDomElementError (Dom.NotFound _) ->
            ( NoDrag, Cmd.none )


ghostStyles : Drag a b -> Maybe ( a, Css.Style )
ghostStyles =
    dragElementAndXY
        >> Maybe.map
            (\{ drag, dragElementOffset, mouseMoveDelta } ->
                let
                    { x, y } =
                        XY.add (XYDelta.diff mouseMoveDelta) dragElementOffset
                in
                ( drag
                , Css.batch
                    [ Styles.absolute
                    , Styles.top_0
                    , Styles.left_0
                    , Css.transform (Css.translate2 (Css.px dragElementOffset.x) (Css.px y))
                    , Css.pointerEvents Css.none
                    ]
                )
            )
