module TodoDict exposing (TodoDict, fromEncodedList, initial, sortedByIdx, toggleCompleted)

import Collection exposing (Collection)
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)
import Todo exposing (Todo)
import TodoId exposing (TodoId)



-- MODEL


type TodoDict
    = TodoDict Internal


type alias Internal =
    Collection TodoId Todo


dict : Collection.System TodoId Todo
dict =
    Collection.system TodoId.toString Todo.id


initial : TodoDict
initial =
    TodoDict dict.empty


fromEncodedList : Value -> Result JD.Error TodoDict
fromEncodedList =
    JD.decodeValue (JD.list Todo.decoder |> JD.map fromList)


fromList : List Todo -> TodoDict
fromList =
    List.foldl insert initial


sortedByIdx : TodoDict -> List Todo
sortedByIdx =
    toList >> List.sortBy Todo.idx


toList : TodoDict -> List Todo
toList =
    unwrap >> dict.values


unwrap (TodoDict internal) =
    internal



-- UPDATE


map : (Internal -> Internal) -> TodoDict -> TodoDict
map func =
    unwrap >> func >> TodoDict


insert : Todo -> TodoDict -> TodoDict
insert todo =
    map (dict.insert todo)


toggleCompleted : TodoId -> TodoDict -> TodoDict
toggleCompleted todoId =
    map (dict.update todoId (Maybe.map Todo.toggle))
