module LabelCollection exposing (LabelCollection, fromEncodedList, initial, sorted, updateSortOrder)

import Collection exposing (Collection)
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)
import Label exposing (Label)
import LabelId exposing (LabelId)



-- MODEL


type LabelCollection
    = LabelCollection Internal


type alias Internal =
    Collection LabelId Label


dict : Collection.System LabelId Label
dict =
    Collection.system LabelId.toString Label.id


initial : LabelCollection
initial =
    LabelCollection dict.empty


fromEncodedList : Value -> Result JD.Error LabelCollection
fromEncodedList =
    JD.decodeValue (JD.list Label.decoder |> JD.map (dict.fromList >> LabelCollection))


sorted : LabelCollection -> List Label
sorted =
    toList >> List.sortBy Label.idx


toList : LabelCollection -> List Label
toList =
    unwrap >> dict.values


unwrap (LabelCollection internal) =
    internal



-- UPDATE


map : (Internal -> Internal) -> LabelCollection -> LabelCollection
map func =
    unwrap >> func >> LabelCollection


updateSortOrder : List Label -> LabelCollection -> LabelCollection
updateSortOrder pl =
    map (\c -> List.indexedMap Label.setIdx pl |> List.foldl dict.insert c)
