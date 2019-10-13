module ProjectCollection exposing (ProjectCollection, fromEncodedList, initial, sortedByIdx)

import Collection exposing (Collection)
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)
import Project exposing (Project)
import ProjectId exposing (ProjectId)



-- MODEL


type ProjectCollection
    = ProjectCollection Internal


type alias Internal =
    Collection ProjectId Project


dict : Collection.System ProjectId Project
dict =
    Collection.system ProjectId.toString Project.id


initial : ProjectCollection
initial =
    ProjectCollection dict.empty


fromEncodedList : Value -> Result JD.Error ProjectCollection
fromEncodedList =
    JD.decodeValue (JD.list Project.decoder |> JD.map (dict.fromList >> ProjectCollection))


sortedByIdx : ProjectCollection -> List Project
sortedByIdx =
    toList >> List.sortBy Project.idx


toList : ProjectCollection -> List Project
toList =
    unwrap >> dict.values


unwrap (ProjectCollection internal) =
    internal



-- UPDATE


map : (Internal -> Internal) -> ProjectCollection -> ProjectCollection
map func =
    unwrap >> func >> ProjectCollection
