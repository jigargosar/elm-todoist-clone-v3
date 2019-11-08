module ReturnRecord exposing (..)


type alias ReturnRecord a x =
    { a : a, list : List x }


singleton : a -> ReturnRecord a x
singleton a =
    ReturnRecord a []


fromTuple : ( a, List x ) -> ReturnRecord a x
fromTuple ( a, list ) =
    ReturnRecord a list


map : (a -> b) -> ReturnRecord a x -> ReturnRecord b x
map func { a, list } =
    fromTuple ( func a, list )
