module PhantomDict exposing (..)

import Dict exposing (Dict)


type PhantomDict k comparable v
    = PhantomDict (Dict comparable v)


unwrap (PhantomDict dict) =
    dict


type alias System k comparable v =
    { empty : PhantomDict k comparable v
    , get : k -> PhantomDict k comparable v -> Maybe v
    }


system : (a -> comparable) -> System k comparable v
system toComparable =
    { empty = PhantomDict Dict.empty
    , get = \k d -> Dict.get (toComparable k) (unwrap d)
    }
