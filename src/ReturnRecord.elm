module ReturnRecord exposing (..)


type alias ReturnRecord a x =
    { a : a, list : List x }


empty : a -> ReturnRecord a x
empty a =
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


add : x -> ReturnRecord a x -> ReturnRecord a x
add x =
    addAll [ x ]


effect : (a -> x) -> ReturnRecord a x -> ReturnRecord a x
effect func ret =
    ret |> add (func ret.a)
