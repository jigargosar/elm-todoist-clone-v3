module TodoDict exposing (TodoDict, empty, fromList, toggle)

import Dict exposing (Dict)
import Todo exposing (Todo)
import TodoId exposing (TodoId)


type TodoDict
    = TodoDict Internal


type alias Internal =
    Dict String Todo


empty : TodoDict
empty =
    TodoDict Dict.empty


fromList : List Todo -> TodoDict
fromList =
    List.foldl insert empty


map : (Internal -> Internal) -> TodoDict -> TodoDict
map func (TodoDict dict) =
    func dict |> TodoDict


insert : Todo -> TodoDict -> TodoDict
insert todo =
    map (TodoId.insert (Todo.id todo) todo)


toggle : TodoId -> TodoDict -> TodoDict
toggle todoId =
    identity
