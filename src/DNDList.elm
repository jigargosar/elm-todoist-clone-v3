module DNDList exposing (DraggingView, Model, Msg, NotDraggingView, View(..), init, subscriptions, update, view)

import Basics.More exposing (Position, flip, msgToCmd, pageXYAsPositionDecoder, rotateListByElem)
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
      --| GettingDragElement (GettingDragElementModel item)
    | GettingDragElement (GettingDragElementState item)
      --| Dragging (DraggingModel item)
    | Dragging (DraggingState item)


type alias GettingDragElementState item =
    { items : List item
    , dragItem : item
    , startPosition : Position
    }


type alias DraggingState item =
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
    = Complete
    | MouseMoved Position
    | DragOver item
    | DragStart (List item) item String Position
    | GotElement (Result Dom.Error Dom.Element)
    | Canceled


update : (Msg item -> msg) -> { onComplete : List item -> msg } -> Msg item -> Model item -> ( Model item, Cmd msg )
update toMsg config message model =
    case ( model, message ) of
        ( _, DragStart items item domId startPosition ) ->
            ( GettingDragElementState items item startPosition
                |> GettingDragElement
            , Dom.getElement domId |> Task.attempt GotElement |> Cmd.map toMsg
            )

        ( _, Canceled ) ->
            ( NotDragging, Cmd.none )

        ( GettingDragElement { items, dragItem, startPosition }, GotElement result ) ->
            case result of
                Ok dragElement ->
                    ( DraggingState items dragItem startPosition dragElement startPosition
                        |> Dragging
                    , Cmd.none
                    )

                Err (Dom.NotFound domId) ->
                    ( model, logError <| "Dom.NotFound domId: " ++ domId )

        ( Dragging ({ items, dragItem } as state), DragOver dragOverItem ) ->
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

        ( Dragging state, MouseMoved currentPosition ) ->
            ( Dragging { state | currentPosition = currentPosition }
            , Cmd.none
            )

        ( Dragging { items }, Complete ) ->
            ( NotDragging
            , config.onComplete items |> msgToCmd
            )

        _ ->
            ( model, Cmd.none )


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
                { dragOverAttrs = \item -> mapAttrList [ E.onMouseOver (DragOver item) ]
                , items = state.items
                , isBeingDragged = always False
                }

        _ ->
            WhenNotDragging
                { dragHandleAttrs =
                    \item domId ->
                        DragStart items item domId
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
                [ Browser.Events.onMouseUp (JD.succeed Complete)
                , Browser.Events.onMouseMove (JD.map MouseMoved pageXYAsPositionDecoder)
                ]
                |> Sub.map toMsg

        _ ->
            Sub.none
