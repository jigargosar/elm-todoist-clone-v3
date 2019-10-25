module DragSort exposing (..)

import Basics.More exposing (eq_, flip, rotateListByElem)
import Browser.Dom as Dom
import Browser.Events
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as JD exposing (Decoder)


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


init : State item -> DragSort item
init =
    DragSort


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


dragHandleAttrs : (Position -> msg) -> List (Attribute msg)
dragHandleAttrs onDragStart =
    [ E.preventDefaultOn "dragstart"
        (JD.map onDragStart pageXYAsPositionDecoder
            |> JD.map (flip Tuple.pair True)
        )
    , A.draggable "true"
    ]
