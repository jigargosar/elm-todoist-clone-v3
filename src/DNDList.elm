module DNDList exposing (..)

import Basics.More exposing (impl)
import Html.Styled exposing (Attribute)


type DNDListState item
    = NotDragging
      --| GettingDragElement (GettingDragElementModel item)
    | GettingDragElement
      --| Dragging (DraggingModel item)
    | Dragging


init : DNDListState item
init =
    impl


type Msg
    = Msg


update : (Msg -> msg) -> { onComplete : List item -> msg } -> Msg -> DNDListState item -> ( DNDListState item, Cmd msg )
update =
    impl


view : (Msg -> msg) -> List items -> DnDListView item msg
view =
    impl


type DnDListView item msg
    = NotDraggingView { dragHandleAttrs : item -> String -> List (Attribute msg) }
    | DraggingView { dragOverAttrs : item -> List (Attribute msg), isBeingDragged : item -> Bool }
