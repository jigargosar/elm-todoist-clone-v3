module TodoDict exposing (TodoDict, fromEncodedList, initial, sortedByIdx, toggleCompleted, withLabelId, withProjectRef)

import Collection exposing (Collection)
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)
import LabelId exposing (LabelId)
import ProjectRef exposing (ProjectRef)
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
    JD.decodeValue (JD.list Todo.decoder |> JD.map (dict.fromList >> TodoDict))


sortedByIdx : TodoDict -> List Todo
sortedByIdx =
    toList >> List.sortBy Todo.idx


toList : TodoDict -> List Todo
toList =
    unwrap >> dict.values


unwrap (TodoDict internal) =
    internal


withProjectRef : ProjectRef -> TodoDict -> List Todo
withProjectRef ref =
    toList >> List.filter (Todo.projectRef >> (==) ref)


withLabelId : LabelId -> TodoDict -> List Todo
withLabelId labelId =
    toList >> List.filter (Todo.hasLabel labelId)



-- UPDATE


map : (Internal -> Internal) -> TodoDict -> TodoDict
map func =
    unwrap >> func >> TodoDict


toggleCompleted : TodoId -> TodoDict -> TodoDict
toggleCompleted todoId =
    map (dict.update todoId (Maybe.map Todo.toggle))
