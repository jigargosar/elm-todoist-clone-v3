module ProjectCollection exposing
    ( ProjectCollection
    , byId
    , fromEncodedList
    , initial
    , put
    , sorted
    , updateSortOrder
    )

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


sorted : ProjectCollection -> List Project
sorted =
    toList >> List.sortBy Project.idx


toList : ProjectCollection -> List Project
toList =
    unwrap >> dict.values


unwrap (ProjectCollection internal) =
    internal


byId : ProjectId -> ProjectCollection -> Maybe Project
byId id =
    unwrap >> dict.get id



-- UPDATE


map : (Internal -> Internal) -> ProjectCollection -> ProjectCollection
map func =
    unwrap >> func >> ProjectCollection


updateSortOrder : List Project -> ProjectCollection -> ProjectCollection
updateSortOrder pl =
    map (\c -> List.indexedMap Project.setIdx pl |> List.foldl dict.insert c)


put : Project -> ProjectCollection -> ProjectCollection
put project =
    map (dict.insert project) >> resort


resort : ProjectCollection -> ProjectCollection
resort model =
    updateSortOrder (sorted model) model
