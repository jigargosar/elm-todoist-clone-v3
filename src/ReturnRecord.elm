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


andThen : (a -> ReturnRecord b x) -> ReturnRecord a x -> ReturnRecord b x
andThen func { a, list } =
    func a
        |> addAll list


addAll : List x -> ReturnRecord a x -> ReturnRecord a x
addAll list_ { a, list } =
    fromTuple ( a, list ++ list_ )
