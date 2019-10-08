module TodoDict exposing (TodoDict, empty, fromList, toggleCompleted)

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
    map (Dict.insert (key todo) todo)


key : Todo -> String
key =
    Todo.id >> TodoId.toDictKey


toggleCompleted : TodoId -> TodoDict -> TodoDict
toggleCompleted todoId =
    map (Dict.update (TodoId.toDictKey todoId) (Maybe.map Todo.toggle))
