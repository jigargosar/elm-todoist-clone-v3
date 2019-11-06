module DragState exposing (..)

import Basics.More exposing (Position, impl)
import Browser.Dom as Dom
import Html.Styled exposing (Attribute)
import Task exposing (Task)


type alias DragStartEvent data =
    { start : Position
    , domId : String
    , data : data
    }


type alias DragState data =
    { data : data
    , dragElement : Dom.Element
    , start : Position
    , current : Position
    }


onDragStart : String -> DragState data -> (DragStartEvent data -> msg)
onDragStart domId handler dragState =
    impl


draggableAttr : DragState data -> Attribute msg
draggableAttr dragState =
    impl


getDragElement : DragStartEvent data -> Task Dom.Error (DragState data)
getDragElement dragEvent =
    impl


dragSubscriptions : DragState data -> Sub msg
dragSubscriptions =
    impl
