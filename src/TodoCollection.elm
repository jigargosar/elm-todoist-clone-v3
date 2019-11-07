module TodoCollection exposing
    ( TodoCollection
    , fromEncodedList
    , inInbox
    , initial
    , sortedByIdx
    , toggleCompleted
    , withLabelId
    , withProjectId
    )

import Collection exposing (Collection)
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)
import LabelId exposing (LabelId)
import ProjectId exposing (ProjectId)
import ProjectRef exposing (ProjectRef)
import Todo exposing (Todo)
import TodoId exposing (TodoId)



-- MODEL


type TodoCollection
    = TodoDict Internal


type alias Internal =
    Collection TodoId Todo


dict : Collection.System TodoId Todo
dict =
    Collection.system TodoId.toString Todo.id


initial : TodoCollection
initial =
    TodoDict dict.empty


fromEncodedList : Value -> Result JD.Error TodoCollection
fromEncodedList =
    JD.decodeValue (JD.list Todo.decoder |> JD.map (dict.fromList >> TodoDict))


sortedByIdx : TodoCollection -> List Todo
sortedByIdx =
    toList >> List.sortBy Todo.idx


toList : TodoCollection -> List Todo
toList =
    unwrap >> dict.values


unwrap (TodoDict internal) =
    internal


withProjectRef : ProjectRef -> TodoCollection -> List Todo
withProjectRef ref =
    toList >> List.filter (Todo.projectRef >> (==) ref)


withProjectId : ProjectId -> TodoCollection -> List Todo
withProjectId projectId =
    let
        ref =
            ProjectRef.fromId projectId
    in
    withProjectRef ref


inInbox : TodoCollection -> List Todo
inInbox =
    withProjectRef ProjectRef.inbox


withLabelId : LabelId -> TodoCollection -> List Todo
withLabelId labelId =
    toList >> List.filter (Todo.hasLabel labelId)



-- UPDATE


map : (Internal -> Internal) -> TodoCollection -> TodoCollection
map func =
    unwrap >> func >> TodoDict


toggleCompleted : TodoId -> TodoCollection -> TodoCollection
toggleCompleted todoId =
    map (dict.update todoId (Maybe.map Todo.toggle))
