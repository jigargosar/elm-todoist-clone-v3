module Drag exposing
    ( Drag
    , Msg
    , XY
    , dragEvents
    , dragIdxInfo
    , dropEvents
    , ghostStyles
    , initial
    , pageXYDecoder
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


type alias XY =
    { x : Float, y : Float }


type Drag
    = NoDrag
    | Drag
        { dragId : String
        , dragIdx : Int
        , startXY : XY
        , currentXY : XY
        , dragElement : Dom.Element
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


dragElementAndXY drag =
    case drag of
        NoDrag ->
            Nothing

        Drag { startXY, currentXY, dragElement } ->
            Just { startXY = startXY, currentXY = currentXY, dragElement = dragElement }

        DragOver { startXY, currentXY, dragElement } ->
            Just { startXY = startXY, currentXY = currentXY, dragElement = dragElement }


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
    | GotDragElement
        { dragId : String
        , dragIdx : Int
        , startXY : XY
        }
        Element
    | GotDropElement Int String Element
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

        Drag _ ->
            getMouseUpOrMove

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

        Drag state ->
            setCurrentXYIn state |> Drag

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
            updateModel message model
    in
    ( newModel, cmd |> Cmd.map toMsg )


updateModel : Msg -> Drag -> ( Drag, Cmd Msg )
updateModel message model =
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
            ( setCurrentXY xy model, Cmd.none )

        GlobalMouseUp ->
            ( NoDrag, Cmd.none )

        MouseDownOnDraggable dragIdx dragId xy ->
            ( model, getElement dragId (GotDragElement { dragId = dragId, dragIdx = dragIdx, startXY = xy }) )

        MouseOverDroppable idx domId ->
            ( model, getElement domId (GotDropElement idx domId) )

        GotDragElement { dragId, dragIdx, startXY } element ->
            ( Drag
                { dragId = dragId
                , dragIdx = dragIdx
                , startXY = startXY
                , currentXY = startXY
                , dragElement = element
                }
            , Cmd.none
            )

        GotDropElement idx domId element ->
            ( case model of
                NoDrag ->
                    model

                Drag { dragId, dragIdx, startXY, currentXY, dragElement } ->
                    DragOver
                        { dragId = dragId
                        , dragIdx = dragIdx
                        , startXY = startXY
                        , currentXY = currentXY
                        , dragElement = dragElement
                        , dropId = domId
                        , dropIdx = idx
                        , dropElement = element
                        }

                DragOver state ->
                    DragOver { state | dropIdx = idx, dropId = domId, dropElement = element }
            , Cmd.none
            )

        GotDomElementError (Dom.NotFound domIdNF) ->
            Debug.todo <| "GotDomElementError: " ++ domIdNF


subtractXY : { a | x : Float, y : Float } -> { b | x : Float, y : Float } -> XY
subtractXY a b =
    XY (a.x - b.x) (a.y - b.y)


addXY a b =
    XY (a.x + b.x) (a.y + b.y)


ghostStyles : Drag -> Css.Style
ghostStyles =
    dragElementAndXY
        >> Maybe.map
            (\{ dragElement, startXY, currentXY } ->
                let
                    { x, y } =
                        addXY (subtractXY currentXY startXY)
                            (subtractXY dragElement.element dragElement.viewport)
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
