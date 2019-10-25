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


init : DragInitPayload item -> DragSort item
init (DragInitPayload state) =
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


type DragStartPayload item
    = DragStartPayload (List item) item String Position


dragHandle : (DragStartPayload item -> msg) -> List item -> item -> String -> List (Attribute msg)
dragHandle msg list_ item domId =
    let
        dragStartMsg : Position -> msg
        dragStartMsg =
            DragStartPayload list_ item domId >> msg
    in
    [ E.preventDefaultOn "dragstart"
        (JD.map dragStartMsg pageXYAsPositionDecoder
            |> JD.map (flip Tuple.pair True)
        )
    , A.draggable "true"
    ]


type DragInitPayload item
    = DragInitPayload (State item)


dragInitTask : DragStartPayload item -> Task Dom.Error (DragInitPayload item)
dragInitTask (DragStartPayload l i d p) =
    Dom.getElement d
        |> Task.map (\el -> State l i el p p |> DragInitPayload)
