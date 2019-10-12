module Collection exposing (Collection, System, system)

import Dict exposing (Dict)


type Collection id v
    = Collection (Dict String v)


unwrap : Collection id v -> Dict String v
unwrap (Collection dict) =
    dict


map : (Dict String v -> Dict String v) -> Collection id v -> Collection id v
map func =
    unwrap >> func >> Collection


type alias System id v =
    { empty : Collection id v
    , get : id -> Collection id v -> Maybe v
    , insert : v -> Collection id v -> Collection id v
    , values : Collection id v -> List v
    , update : id -> (Maybe v -> Maybe v) -> Collection id v -> Collection id v
    }


system : (id -> String) -> (v -> id) -> System id v
system idToString getId =
    let
        valueToIdString =
            getId >> idToString
    in
    { empty = Collection Dict.empty
    , get = \id -> unwrap >> Dict.get (idToString id)
    , insert = \v -> map (Dict.insert (valueToIdString v) v)
    , values = unwrap >> Dict.values
    , update = \id func -> map (Dict.update (idToString id) func)
    }
