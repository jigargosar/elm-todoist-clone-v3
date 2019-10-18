module Drag exposing
    ( Drag
    , Info
    , Msg
    , System
    , dragEvents
    , dropEvents
    , eqDragOverIdx
    , ghostStyles
    , info
    , initial
    , rotate
    , rotateFromInfo
    , subscriptions
    , system
    , update
    )

import Basics.More exposing (flip)
import Browser.Dom as Dom exposing (Element)
import Browser.Events as BE
import Css
import Html.Styled as H
import Html.Styled.Events as E
import Json.Decode as JD
import SelectList
import Styles
import Task
import XY exposing (XY)
import XYDelta exposing (XYDelta)


type alias System a msg =
    { initial : Drag
    , update : Msg -> Drag -> ( Drag, Cmd msg )
    , subscriptions : Drag -> Sub msg
    , dragEvents : Int -> String -> Drag -> List (H.Attribute msg)
    , dropEvents : Int -> Drag -> List (H.Attribute msg)
    , eqDragOverIdx : Int -> Drag -> Bool
    , info : Drag -> Maybe Info
    , rotate : Drag -> List a -> List a
    , ghostStyles : Drag -> Maybe ( Int, Css.Style )
    }


system : (Msg -> msg) -> (Info -> msg) -> System a msg
system toMsg onChange =
    { initial = initial
    , update = update toMsg onChange
    , subscriptions = subscriptions toMsg
    , dragEvents = dragEvents toMsg
    , dropEvents = dropEvents toMsg
    , eqDragOverIdx = eqDragOverIdx
    , info = info
    , rotate = rotate
    , ghostStyles = ghostStyles
    }


type alias Info =
    { drag : Int, dragOver : Int }


info : Drag -> Maybe Info
info (Drag internal) =
    internal
        |> Maybe.map
            (\state -> { drag = state.drag, dragOver = state.dragOver })


type Drag
    = Drag (Maybe State)


rotateFromInfo : Info -> List a -> List a
rotateFromInfo { drag, dragOver } list =
    SelectList.fromList list
        |> Maybe.andThen (SelectList.selectBy drag)
        |> Maybe.map (SelectList.moveBy (dragOver - drag) >> SelectList.toList)
        |> Maybe.withDefault list


eqDragOverIdx : Int -> Drag -> Bool
eqDragOverIdx idx =
    info
        >> Maybe.map
            (\{ drag, dragOver } ->
                idx == dragOver
            )
        >> Maybe.withDefault False


rotate : Drag -> List a -> List a
rotate drag =
    case info drag of
        Nothing ->
            identity

        Just inf ->
            rotateFromInfo inf


type alias State =
    { drag : Int
    , mouseMoveDelta : XYDelta
    , dragElementOffset : XY
    , dragOver : Int
    }


initial : Drag
initial =
    Drag Nothing


dragElementAndXY (Drag internal) =
    internal
        |> Maybe.map
            (\{ drag, mouseMoveDelta, dragElementOffset } ->
                { drag = drag, mouseMoveDelta = mouseMoveDelta, dragElementOffset = dragElementOffset }
            )


type Msg
    = GlobalMouseMove XY
    | GlobalMouseUp
    | MouseDownOnDraggable Int String XY
    | MouseOverDroppable Int
    | GotDragElement Int XY Element
    | GotDomElementError Dom.Error


subscriptions : (Msg -> msg) -> Drag -> Sub msg
subscriptions toMsg (Drag internal) =
    let
        subs =
            Sub.batch
                [ BE.onMouseMove (JD.map GlobalMouseMove XY.pageXYDecoder)
                , BE.onMouseUp (JD.succeed GlobalMouseUp)
                ]
                |> Sub.map toMsg
    in
    case internal of
        Nothing ->
            Sub.none

        Just _ ->
            subs


xyMovedTo : XY -> Drag -> Drag
xyMovedTo xy (Drag internal) =
    let
        setCurrentXYIn state =
            { state | mouseMoveDelta = XYDelta.moveTo xy state.mouseMoveDelta }
    in
    internal |> Maybe.map setCurrentXYIn |> Drag


dragEvents : (Msg -> msg) -> Int -> String -> Drag -> List (H.Attribute msg)
dragEvents tagger idx domId (Drag internal) =
    case internal of
        Nothing ->
            [ E.preventDefaultOn "mousedown"
                (XY.pageXYDecoder
                    |> JD.map (MouseDownOnDraggable idx domId >> tagger >> pd)
                )
            ]

        Just _ ->
            []


dropEvents : (Msg -> msg) -> Int -> Drag -> List (H.Attribute msg)
dropEvents tagger idx (Drag internal) =
    case internal of
        Nothing ->
            []

        Just _ ->
            [ E.onMouseOver (MouseOverDroppable idx |> tagger) ]


pd =
    flip Tuple.pair False


update : (Msg -> msg) -> (Info -> msg) -> Msg -> Drag -> ( Drag, Cmd msg )
update toMsg onChange message ((Drag internal) as model) =
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
                |> Cmd.map toMsg
    in
    case message of
        GlobalMouseMove xy ->
            ( xyMovedTo xy model, Cmd.none )

        GlobalMouseUp ->
            ( Drag Nothing, Cmd.none )

        MouseDownOnDraggable dragIdx dragId xy ->
            ( model
            , getElement dragId (GotDragElement dragIdx xy)
            )

        MouseOverDroppable b ->
            ( case internal of
                Nothing ->
                    model

                Just state ->
                    Drag (Just { state | dragOver = b })
            , Cmd.none
            )

        GotDragElement a xy element ->
            ( Drag
                (Just
                    { drag = a
                    , mouseMoveDelta = XYDelta.init xy
                    , dragElementOffset = XY.subtract element.element element.viewport
                    , dragOver = a
                    }
                )
            , Cmd.none
            )

        GotDomElementError (Dom.NotFound _) ->
            ( Drag Nothing
            , info model
                |> Maybe.map (onChange >> Task.succeed >> Task.perform identity)
                |> Maybe.withDefault Cmd.none
            )


ghostStyles : Drag -> Maybe ( Int, Css.Style )
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
