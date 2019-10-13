module ProjectDict exposing (ProjectDict, fromEncodedList, initial, sortedByIdx)

import Collection exposing (Collection)
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)
import Project exposing (Project)
import ProjectId exposing (ProjectId)



-- MODEL


type ProjectDict
    = ProjectDict Internal


type alias Internal =
    Collection ProjectId Project


dict : Collection.System ProjectId Project
dict =
    Collection.system ProjectId.toString Project.id


initial : ProjectDict
initial =
    ProjectDict dict.empty


fromEncodedList : Value -> Result JD.Error ProjectDict
fromEncodedList =
    JD.decodeValue (JD.list Project.decoder |> JD.map (dict.fromList >> ProjectDict))


sortedByIdx : ProjectDict -> List Project
sortedByIdx =
    toList >> List.sortBy Project.idx


toList : ProjectDict -> List Project
toList =
    unwrap >> dict.values


unwrap (ProjectDict internal) =
    internal



-- UPDATE


map : (Internal -> Internal) -> ProjectDict -> ProjectDict
map func =
    unwrap >> func >> ProjectDict
