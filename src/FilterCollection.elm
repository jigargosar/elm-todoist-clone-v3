module FilterCollection exposing (FilterCollection, byId, fromEncodedList, initial, sorted, updateSortOrder)

import Collection exposing (Collection)
import Filter exposing (Filter)
import FilterId exposing (FilterId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)



-- MODEL


type FilterCollection
    = FilterCollection Internal


type alias Internal =
    Collection FilterId Filter


dict : Collection.System FilterId Filter
dict =
    Collection.system FilterId.toString Filter.id


initial : FilterCollection
initial =
    FilterCollection dict.empty


fromEncodedList : Value -> Result JD.Error FilterCollection
fromEncodedList =
    JD.decodeValue (JD.list Filter.decoder |> JD.map (dict.fromList >> FilterCollection))


sorted : FilterCollection -> List Filter
sorted =
    toList >> List.sortBy Filter.idx


toList : FilterCollection -> List Filter
toList =
    unwrap >> dict.values


unwrap (FilterCollection internal) =
    internal


byId : FilterId -> FilterCollection -> Maybe Filter
byId id =
    unwrap >> dict.get id



-- UPDATE


map : (Internal -> Internal) -> FilterCollection -> FilterCollection
map func =
    unwrap >> func >> FilterCollection


updateSortOrder : List Filter -> FilterCollection -> FilterCollection
updateSortOrder pl =
    map (\c -> List.indexedMap Filter.setIdx pl |> List.foldl dict.insert c)
