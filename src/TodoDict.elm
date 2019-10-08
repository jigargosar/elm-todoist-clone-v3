module TodoDict exposing (TodoDict, empty, fromList, toList, toggleCompleted)

import Dict exposing (Dict)
import Todo exposing (Todo)
import TodoId exposing (TodoId)



-- MODEL


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


toList : TodoDict -> List Todo
toList =
    unwrap >> Dict.values


unwrap (TodoDict dict) =
    dict


key : Todo -> String
key =
    Todo.id >> TodoId.toDictKey



-- UPDATE


map : (Internal -> Internal) -> TodoDict -> TodoDict
map func =
    unwrap >> func >> TodoDict


insert : Todo -> TodoDict -> TodoDict
insert todo =
    map (Dict.insert (key todo) todo)


toggleCompleted : TodoId -> TodoDict -> TodoDict
toggleCompleted todoId =
    map (Dict.update (TodoId.toDictKey todoId) (Maybe.map Todo.toggle))
