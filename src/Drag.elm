module Drag exposing
    ( Drag
    , Msg
    , dragEvents
    , dragIdxInfo
    , dropEvents
    , dropIdxEq
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
        , dragElement : Dom.Element
        }
    | DragOver
        { dragIdx : Int
        , xyDelta : XYDelta
        , dragElement : Dom.Element
        , dropIdx : Int
        }


initial : Drag
initial =
    NoDrag


dragElementAndXY drag =
    case drag of
        NoDrag ->
            Nothing

        Drag { xyDelta, dragElement } ->
            Just { xyDelta = xyDelta, dragElement = dragElement }

        DragOver { xyDelta, dragElement } ->
            Just { xyDelta = xyDelta, dragElement = dragElement }


dropIdxEq : Int -> Drag -> Bool
dropIdxEq idx =
    dragIdxInfo >> Maybe.map (.dragIdx >> (==) idx) >> Maybe.withDefault False


dragIdxInfo : Drag -> Maybe { dragIdx : Int, dropIdx : Int }
dragIdxInfo model =
    case model of
        NoDrag ->
            Nothing

        Drag { dragIdx } ->
            Just { dragIdx = dragIdx, dropIdx = dragIdx }

        DragOver { dragIdx, dropIdx } ->
            Just { dragIdx = dragIdx, dropIdx = dropIdx }


type Msg
    = GlobalMouseMove XY
    | GlobalMouseUp
    | MouseDownOnDraggable Int String XY
    | MouseOverDroppable Int String
    | GotDragElement Int XY Element
    | GotDropElement Int String Element
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


dropEvents : (Msg -> msg) -> Int -> String -> Drag -> List (H.Attribute msg)
dropEvents tagger idx domId model =
    let
        events =
            [ E.onMouseOver (MouseOverDroppable idx domId |> tagger) ]
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

        MouseOverDroppable idx domId ->
            ( model, getElement domId (GotDropElement idx domId) )

        GotDragElement dragIdx xy element ->
            ( Drag
                { dragIdx = dragIdx
                , xyDelta = XYDelta.init xy
                , dragElement = element
                }
            , Cmd.none
            )

        GotDropElement idx domId element ->
            ( case model of
                NoDrag ->
                    model

                Drag { dragIdx, xyDelta, dragElement } ->
                    DragOver
                        { dragIdx = dragIdx
                        , xyDelta = xyDelta
                        , dragElement = dragElement
                        , dropIdx = idx
                        }

                DragOver state ->
                    DragOver
                        { state
                            | dropIdx = idx
                        }
            , Cmd.none
            )

        GotDomElementError (Dom.NotFound _) ->
            ( NoDrag, Cmd.none )


ghostStyles : Drag -> Css.Style
ghostStyles =
    dragElementAndXY
        >> Maybe.map
            (\{ dragElement, xyDelta } ->
                let
                    { x, y } =
                        XY.add (XYDelta.diff xyDelta)
                            (XY.subtract dragElement.element dragElement.viewport)
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
