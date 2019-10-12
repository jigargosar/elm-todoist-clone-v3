module TodoDict exposing (TodoDict, fromEncodedList, initial, sortedByIdx, toList, toggleCompleted)

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)
import PhantomDict exposing (PhantomDict)
import Todo exposing (Todo)
import TodoId exposing (TodoId)



-- MODEL


type TodoDict
    = TodoDict Internal


type alias Internal =
    PhantomDict TodoId String Todo


phantomDictS : PhantomDict.System TodoId String Todo
phantomDictS =
    PhantomDict.system TodoId.toString


initial : TodoDict
initial =
    TodoDict phantomDictS.empty


fromEncodedList : Value -> Result JD.Error TodoDict
fromEncodedList =
    JD.decodeValue (JD.list Todo.decoder |> JD.map fromList)


fromList : List Todo -> TodoDict
fromList =
    List.foldl insert initial


toList : TodoDict -> List Todo
toList =
    unwrap >> phantomDictS.values


sortedByIdx : TodoDict -> List Todo
sortedByIdx =
    toList >> List.sortBy Todo.idx


unwrap (TodoDict internal) =
    internal



-- UPDATE


map : (Internal -> Internal) -> TodoDict -> TodoDict
map func =
    unwrap >> func >> TodoDict


insert : Todo -> TodoDict -> TodoDict
insert todo =
    map (phantomDictS.insert (Todo.id todo) todo)


toggleCompleted : TodoId -> TodoDict -> TodoDict
toggleCompleted todoId =
    map (phantomDictS.update todoId (Maybe.map Todo.toggle))
