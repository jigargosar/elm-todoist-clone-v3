module DragSort exposing (..)

import Basics.More exposing (eq_, flip, onDomErrorRecover, rotateListByElem)
import Browser.Dom as Dom
import Browser.Events
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as JD exposing (Decoder)
import Task exposing (Task)


type alias Position =
    { x : Int, y : Int }


type DragSort item
    = DragSort (State item)


type alias State item =
    { list : List item
    , drag : item
    , dragEl : Dom.Element
    , start : Position
    , current : Position
    }


init : Init_Step_2 item -> DragSort item
init (Init_Step_2 state) =
    DragSort state


sortOnDragOver : item -> DragSort item -> DragSort item
sortOnDragOver dragOver =
    map
        (\model ->
            if dragOver == model.drag then
                model

            else
                let
                    newProjectList =
                        rotateListByElem model.drag dragOver model.list
                            |> Maybe.withDefault model.list
                in
                { model | list = newProjectList }
        )


unwrap : DragSort item -> State item
unwrap (DragSort state) =
    state


map : (State item -> State a) -> DragSort item -> DragSort a
map func =
    unwrap >> func >> DragSort


setCurrent : Position -> DragSort item -> DragSort item
setCurrent position =
    map (\state -> { state | current = position })


list : DragSort item -> List item
list =
    unwrap >> .list


isBeingDragged : item -> DragSort item -> Bool
isBeingDragged item =
    unwrap >> .drag >> eq_ item


subscriptions : { currentChanged : Position -> msg, done : msg } -> DragSort item -> Sub msg
subscriptions { currentChanged, done } _ =
    Sub.batch
        [ Browser.Events.onMouseUp (JD.succeed done)
        , Browser.Events.onMouseMove (JD.map currentChanged pageXYAsPositionDecoder)
        ]


pageXYAsPositionDecoder : Decoder Position
pageXYAsPositionDecoder =
    JD.map2 Position
        (JD.field "pageX" JD.int)
        (JD.field "pageY" JD.int)


type Init_Step_1 item
    = Init_Step_1 (List item) item String Position


dragHandle : (Init_Step_1 item -> msg) -> List item -> item -> String -> List (Attribute msg)
dragHandle msg list_ item domId =
    let
        dragStartMsg : Position -> msg
        dragStartMsg =
            Init_Step_1 list_ item domId >> msg
    in
    [ E.preventDefaultOn "dragstart"
        (JD.map dragStartMsg pageXYAsPositionDecoder
            |> JD.map (flip Tuple.pair True)
        )
    , A.draggable "true"
    ]


type Init_Step_2 item
    = Init_Step_2 (State item)


dragInitTask : Init_Step_1 item -> Task Dom.Error (Init_Step_2 item)
dragInitTask (Init_Step_1 l i d p) =
    Dom.getElement d
        |> Task.map (\el -> State l i el p p |> Init_Step_2)
