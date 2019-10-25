module DragSort exposing (..)

import Basics.More exposing (rotateListByElem)
import Browser.Dom as Dom


type alias Position =
    { x : Int, y : Int }


type alias DragSort item =
    { list : List item
    , drag : item
    , dragEl : Dom.Element
    , start : Position
    , current : Position
    }


sortOnDragOver dragOver model =
    if dragOver == model.drag then
        model

    else
        let
            newProjectList =
                rotateListByElem model.drag dragOver model.list
                    |> Maybe.withDefault model.list
        in
        { model | list = newProjectList }


setCurrent : Position -> DragSort item -> DragSort item
setCurrent position model =
    { model | current = position }
