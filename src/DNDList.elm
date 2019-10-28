module DNDList exposing
    ( Config
    , DraggingConfig
    , Model
    , Msg
    , NotDraggingConfig
    , System
    , View2
    , ghost
    , initial
    , subscriptions
    , system
    , update
    , view2
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


type alias System item msg =
    { initial : Model item
    , update : Msg item -> Model item -> ( Model item, Cmd msg )
    , subscriptions : Model item -> Sub msg
    , view : List item -> Model item -> View2 item msg
    }


system : Config item msg -> System item msg
system config =
    { initial = initial
    , update = update config
    , subscriptions = subscriptions config
    , view = view2 config
    }


type alias Config item msg =
    { toMsg : Msg item -> msg
    , sorted : List item -> msg
    }


type Model item
    = NotDragging
    | Dragging (State item)


type alias DragStart item =
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
    = DragStarted (DragStart item)
    | GotElement (DragStart item) (Result Dom.Error Dom.Element)
    | Canceled
    | Completed
    | MouseMoved Position
    | DraggedOver item


update :
    Config item msg
    -> Msg item
    -> Model item
    -> ( Model item, Cmd msg )
update config message model =
    case message of
        DragStarted payload ->
            ( model
            , Dom.getElement payload.dragItemDomId
                |> Task.attempt (GotElement payload)
                |> Cmd.map config.toMsg
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
            updateDragging
                (\{ items } ->
                    ( NotDragging
                    , config.sorted items |> msgToCmd
                    )
                )
                model

        MouseMoved currentPosition ->
            ( map (\state -> { state | currentPosition = currentPosition }) model
            , Cmd.none
            )

        DraggedOver dragOverItem ->
            ( map (sortItemsOnDragOver dragOverItem) model
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


updateDragging : (State item -> ( Model item, Cmd msg )) -> Model item -> ( Model item, Cmd msg )
updateDragging func model =
    case model of
        NotDragging ->
            ( model, Cmd.none )

        Dragging state ->
            func state


map : (State item -> State item) -> Model item -> Model item
map func model =
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


type alias View2 item msg =
    { dragHandleAttrs : item -> String -> List (Attribute msg)
    , dragOverAttrs : item -> List (Attribute msg)
    , isBeingDragged : item -> Bool
    , items : List item
    }


view2 : Config item msg -> List item -> Model item -> View2 item msg
view2 config items model =
    let
        attrsToMsg =
            List.map (A.map config.toMsg)
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
                    (DragStarted << DragStart items item domId)
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


type alias NotDraggingConfig item msg =
    { dragHandleAttrs : item -> String -> List (Attribute msg)
    , items : List item
    }


type alias DraggingConfig item msg =
    { dragOverAttrs : item -> List (Attribute msg)
    , isBeingDragged : item -> Bool
    , items : List item
    , ghost : ( Style, item )
    }


subscriptions : Config item msg -> Model item -> Sub msg
subscriptions config model =
    case model of
        Dragging _ ->
            Sub.batch
                [ Browser.Events.onMouseUp (JD.succeed Completed)
                , Browser.Events.onMouseMove (JD.map MouseMoved pageXYAsPositionDecoder)
                ]
                |> Sub.map config.toMsg

        _ ->
            Sub.none
