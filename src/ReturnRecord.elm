module ReturnRecord exposing (..)


type alias ReturnRecord a x =
    { a : a, list : List x }


only : a -> ReturnRecord a x
only a =
    ReturnRecord a []


fromTuple : ( a, List x ) -> ReturnRecord a x
fromTuple ( a, list ) =
    ReturnRecord a list


map : (a -> b) -> ReturnRecord a x -> ReturnRecord b x
map func { a, list } =
    fromTuple ( func a, list )


andThen : (a -> ReturnRecord b x) -> ReturnRecord a x -> ReturnRecord b x
andThen func ret =
    let
        { a, list } =
            func ret.a
    in
    fromTuple ( a, ret.list ++ list )


addAll : List x -> ReturnRecord a x -> ReturnRecord a x
addAll list_ { a, list } =
    fromTuple ( a, list ++ list_ )


add : x -> ReturnRecord a x -> ReturnRecord a x
add x =
    addAll [ x ]


addEffect : (a -> x) -> ReturnRecord a x -> ReturnRecord a x
addEffect func ret =
    ret |> add (func ret.a)
