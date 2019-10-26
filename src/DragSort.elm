module DragSort exposing (..)

import Basics.More exposing (eq_, flip, rotateListByElem)
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


initStep_2 : InitContext_2 item -> DragSort item
initStep_2 (InitContext_2 state) =
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


type InitContext item
    = InitContext (List item) item String Position


dragHandle : (InitContext item -> msg) -> List item -> item -> String -> List (Attribute msg)
dragHandle msg list_ item domId =
    let
        dragStartMsg : Position -> msg
        dragStartMsg =
            InitContext list_ item domId >> msg
    in
    [ E.preventDefaultOn "dragstart"
        (JD.map dragStartMsg pageXYAsPositionDecoder
            |> JD.map (flip Tuple.pair True)
        )
    , A.draggable "true"
    ]


type InitContext_2 item
    = InitContext_2 (State item)


initStep_1_GetDragElement : InitContext item -> Task Dom.Error (InitContext_2 item)
initStep_1_GetDragElement (InitContext l i d p) =
    Dom.getElement d
        |> Task.map (\el -> State l i el p p |> InitContext_2)



--


type DNDListState item
    = NotDragging
      --| GettingDragElement (GettingDragElementModel item)
    | GettingDragElement
      --| Dragging (DraggingModel item)
    | Dragging


type alias GettingDragElementModel item =
    { list : List item
    , dragItem : item
    , dragStartedAt : Position
    }


type alias DraggingModel item =
    { list : List item
    , dragItem : item
    , dragStartedAt : Position
    , dragElement : Dom.Element
    , mouseAt : Position
    }


type DNDListEvents
    = OnDragStart
    | OnDragOver
    | OnMouseMoved
    | OnCancel
    | OnComplete
    | GotDragElement
    | GotDragElementError


stateTransitions event state =
    case ( state, event ) of
        ( _, OnDragStart ) ->
            GettingDragElement

        ( _, OnCancel ) ->
            NotDragging

        ( GettingDragElement, GotDragElement ) ->
            Dragging

        ( GettingDragElement, GotDragElementError ) ->
            NotDragging

        ( Dragging, OnDragOver ) ->
            Dragging

        ( Dragging, OnMouseMoved ) ->
            Dragging

        ( Dragging, OnComplete ) ->
            NotDragging

        _ ->
            state
