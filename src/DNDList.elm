module DNDList exposing (DraggingView, Model, Msg, NotDraggingView, View(..), init, subscriptions, update, view)

import Basics.More exposing (Position, flip, pageXYAsPositionDecoder)
import Browser.Events
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as JD


type Model item
    = NotDragging
      --| GettingDragElement (GettingDragElementModel item)
    | GettingDragElement
      --| Dragging (DraggingModel item)
    | Dragging


init : Model item
init =
    NotDragging


type Msg item
    = Complete
    | MouseMoved Position
    | DragOver item
    | DragStart (List item) item String Position


update : (Msg item -> msg) -> { onComplete : List item -> msg } -> Msg item -> Model item -> ( Model item, Cmd msg )
update toMsg config message model =
    case message of
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
        Dragging ->
            WhenDragging
                { dragOverAttrs = \item -> mapAttrList [ E.onMouseOver (DragOver item) ]
                , items = items
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
        Dragging ->
            Sub.batch
                [ Browser.Events.onMouseUp (JD.succeed Complete)
                , Browser.Events.onMouseMove (JD.map MouseMoved pageXYAsPositionDecoder)
                ]
                |> Sub.map toMsg

        _ ->
            Sub.none
