module DNDList exposing
    ( Config
    , Model
    , Msg
    , View
    , ghost
    , initial
    , subscriptions
    , update
    , view
    )

import Basics.More
    exposing
        ( Position
        , eq_
        , flip
        , msgToCmd
        , pageXYAsPositionDecoder
        , positionAdd
        , positionDiff
        , rotateListByElem
        )
import Browser.Dom as Dom
import Browser.Events
import Css exposing (Style)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as JD
import Log exposing (logError)
import Styles exposing (batch)
import Task


type alias Config item msg =
    { toMsg : Msg item -> msg
    , sorted : List item -> msg
    }


type Model item
    = NotDragging
    | Dragging (State item)


type alias DragStarted_ item =
    { items : List item
    , dragItem : item
    , dragItemDomId : String
    , startPosition : Position
    }


type alias State item =
    { items : List item
    , dragItem : item
    , startPosition : Position
    , dragElement : Dom.Element
    , currentPosition : Position
    }


initial : Model item
initial =
    NotDragging


type Msg item
    = DragStarted (DragStarted_ item)
    | GotElement (DragStarted_ item) (Result Dom.Error Dom.Element)
    | Canceled
    | Completed
    | MouseMoved Position
    | DraggedOver item


update :
    Config item msg
    -> Msg item
    -> Model item
    -> ( Model item, Cmd msg )
update { toMsg, sorted } message model =
    case message of
        DragStarted payload ->
            ( model
            , Dom.getElement payload.dragItemDomId
                |> Task.attempt (GotElement payload)
                |> Cmd.map toMsg
            )

        Canceled ->
            ( NotDragging, Cmd.none )

        GotElement { items, dragItem, startPosition } result ->
            case result of
                Ok dragElement ->
                    ( State items dragItem startPosition dragElement startPosition
                        |> Dragging
                    , Cmd.none
                    )

                Err (Dom.NotFound domId) ->
                    ( model, logError <| "Dom.NotFound domId: " ++ domId )

        Completed ->
            updateState
                (\{ items } ->
                    ( NotDragging
                    , sorted items |> msgToCmd
                    )
                )
                model

        MouseMoved currentPosition ->
            ( mapState (\state -> { state | currentPosition = currentPosition }) model
            , Cmd.none
            )

        DraggedOver dragOverItem ->
            ( mapState (sortItemsOnDragOver dragOverItem) model
            , Cmd.none
            )


sortItemsOnDragOver : item -> State item -> State item
sortItemsOnDragOver dragOverItem state =
    if dragOverItem == state.dragItem then
        state

    else
        { state
            | items =
                rotateListByElem state.dragItem dragOverItem state.items
                    |> Maybe.withDefault state.items
        }


updateState : (State item -> ( Model item, Cmd msg )) -> Model item -> ( Model item, Cmd msg )
updateState func model =
    case model of
        NotDragging ->
            ( model, Cmd.none )

        Dragging state ->
            func state


mapState : (State item -> State item) -> Model item -> Model item
mapState func model =
    case model of
        Dragging state ->
            func state |> Dragging

        _ ->
            model


dragHandleAttrs : (Position -> msg) -> List (Attribute msg)
dragHandleAttrs dragStartMsg =
    [ E.preventDefaultOn "dragstart"
        (JD.map dragStartMsg pageXYAsPositionDecoder
            |> JD.map (flip Tuple.pair True)
        )
    , A.draggable "true"
    ]


type alias View item msg =
    { dragHandleAttrs : item -> String -> List (Attribute msg)
    , dragOverAttrs : item -> List (Attribute msg)
    , isBeingDragged : item -> Bool
    , items : List item
    }


view : { a | toMsg : Msg item -> msg } -> List item -> Model item -> View item msg
view { toMsg } items model =
    let
        attrsToMsg =
            List.map (A.map toMsg)
    in
    case model of
        Dragging state ->
            { dragOverAttrs = \item -> attrsToMsg [ E.onMouseOver (DraggedOver item) ]
            , dragHandleAttrs = \_ _ -> []
            , isBeingDragged = eq_ state.dragItem
            , items = state.items
            }

        NotDragging ->
            { dragOverAttrs = \_ -> []
            , dragHandleAttrs =
                \item domId ->
                    (DragStarted << DragStarted_ items item domId)
                        |> dragHandleAttrs
                        |> attrsToMsg
            , isBeingDragged = always False
            , items = items
            }


getState model =
    case model of
        NotDragging ->
            Nothing

        Dragging state ->
            Just state


ghost : Model a -> Maybe ( Css.Style, a )
ghost =
    getState
        >> Maybe.map stateToGhost


stateToGhost { dragItem, dragElement, startPosition, currentPosition } =
    let
        dragElementOffset =
            dragElement.element

        { x, y } =
            positionAdd (positionDiff currentPosition startPosition) dragElementOffset
    in
    ( batch
        [ Styles.absolute
        , Styles.top_0
        , Styles.left_0
        , Css.transform (Css.translate2 (Css.px dragElementOffset.x) (Css.px y))
        , Css.pointerEvents Css.none
        ]
    , dragItem
    )


subscriptions : { a | toMsg : Msg item -> msg } -> Model item -> Sub msg
subscriptions { toMsg } model =
    case model of
        Dragging _ ->
            Sub.batch
                [ Browser.Events.onMouseUp (JD.succeed Completed)
                , Browser.Events.onMouseMove (JD.map MouseMoved pageXYAsPositionDecoder)
                ]
                |> Sub.map toMsg

        _ ->
            Sub.none
