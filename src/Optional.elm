module Optional exposing (..)


type alias Optional s b =
    { get : b -> Maybe s
    , set : s -> b -> b
    }


fromTuple : ( b -> Maybe s, s -> b -> b ) -> Optional s b
fromTuple ( get, set ) =
    Optional get set


map : Optional s b -> (s -> s) -> b -> b
map { get, set } func big =
    case get big of
        Just small ->
            set (func small) big

        Nothing ->
            big
