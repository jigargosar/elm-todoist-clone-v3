module PhantomDict exposing (PhantomDict, System, system)

import Dict exposing (Dict)


type PhantomDict k comparable v
    = PhantomDict (Dict comparable v)


unwrap : PhantomDict k comparable v -> Dict comparable v
unwrap (PhantomDict dict) =
    dict


map : (Dict comparable v -> Dict comparable v) -> PhantomDict k comparable v -> PhantomDict k comparable v
map func =
    unwrap >> func >> PhantomDict


type alias System k comparable v =
    { empty : PhantomDict k comparable v
    , get : k -> PhantomDict k comparable v -> Maybe v
    , insert : k -> v -> PhantomDict k comparable v -> PhantomDict k comparable v
    , values : PhantomDict k comparable v -> List v
    , update : k -> (Maybe v -> Maybe v) -> PhantomDict k comparable v -> PhantomDict k comparable v
    }


system : (a -> comparable) -> System k comparable v
system toComp =
    { empty = PhantomDict Dict.empty
    , get = \k -> unwrap >> Dict.get (toComp k)
    , insert = \k v -> map (Dict.insert (toComp k) v)
    , values = unwrap >> Dict.values
    , update = \k func -> map (Dict.update (toComp k) func)
    }
