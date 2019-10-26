module DNDList exposing (DraggingView, Model, Msg, NotDraggingView, View(..), init, subscriptions, update, view)

import Basics.More exposing (Position, eq_, flip, msgToCmd, pageXYAsPositionDecoder, rotateListByElem)
import Browser.Dom as Dom
import Browser.Events
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as JD
import Log exposing (logError)
import Task


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


init : Model item
init =
    NotDragging


type Msg item
    = DragStarted (DragStart item)
    | GotElement (DragStart item) (Result Dom.Error Dom.Element)
    | Canceled
    | WhileDragging (DraggingMsg item)


type DraggingMsg item
    = Completed
    | MouseMoved Position
    | DraggedOver item


update : (Msg item -> msg) -> { onComplete : List item -> msg } -> Msg item -> Model item -> ( Model item, Cmd msg )
update toMsg config message model =
    case message of
        DragStarted dragStart ->
            ( model
            , Dom.getElement dragStart.dragItemDomId |> Task.attempt (GotElement dragStart) |> Cmd.map toMsg
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

        WhileDragging msg ->
            case model of
                NotDragging ->
                    ( model, Cmd.none )

                Dragging ({ items, dragItem } as state) ->
                    case msg of
                        Completed ->
                            ( NotDragging
                            , config.onComplete items |> msgToCmd
                            )

                        MouseMoved currentPosition ->
                            ( Dragging { state | currentPosition = currentPosition }
                            , Cmd.none
                            )

                        DraggedOver dragOverItem ->
                            ( if dragOverItem == dragItem then
                                model

                              else
                                Dragging
                                    { state
                                        | items =
                                            rotateListByElem dragItem dragOverItem items
                                                |> Maybe.withDefault items
                                    }
                            , Cmd.none
                            )


dragHandleAttrs : (Position -> msg) -> List (Attribute msg)
dragHandleAttrs dragStartMsg =
    [ E.preventDefaultOn "dragstart"
        (JD.map dragStartMsg pageXYAsPositionDecoder
            |> JD.map (flip Tuple.pair True)
        )
    , A.draggable "true"
    ]


view : (Msg item -> msg) -> List item -> Model item -> View item msg
view toMsg items model =
    let
        mapAttrList =
            List.map (A.map toMsg)
    in
    case model of
        Dragging state ->
            WhenDragging
                { dragOverAttrs = \item -> mapAttrList [ E.onMouseOver (WhileDragging <| DraggedOver item) ]
                , items = state.items
                , isBeingDragged = eq_ state.dragItem
                }

        _ ->
            WhenNotDragging
                { dragHandleAttrs =
                    \item domId ->
                        (DragStarted << DragStart items item domId)
                            |> dragHandleAttrs
                            |> mapAttrList
                , items = items
                }


type alias NotDraggingView item msg =
    { dragHandleAttrs : item -> String -> List (Attribute msg)
    , items : List item
    }


type alias DraggingView item msg =
    { dragOverAttrs : item -> List (Attribute msg)
    , isBeingDragged : item -> Bool
    , items : List item
    }


type View item msg
    = WhenNotDragging (NotDraggingView item msg)
    | WhenDragging (DraggingView item msg)


subscriptions : (Msg item -> msg) -> Model item -> Sub msg
subscriptions toMsg model =
    case model of
        Dragging _ ->
            Sub.batch
                [ Browser.Events.onMouseUp (JD.succeed Completed)
                , Browser.Events.onMouseMove (JD.map MouseMoved pageXYAsPositionDecoder)
                ]
                |> Sub.map (WhileDragging >> toMsg)

        _ ->
            Sub.none
