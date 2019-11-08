module Ret exposing (..)


type alias Ret a x =
    { a : a, list : List x }


only : a -> Ret a x
only a =
    Ret a []


fromTuple : ( a, List x ) -> Ret a x
fromTuple ( a, list ) =
    Ret a list


batch : Ret a (Cmd msg) -> ( a, Cmd msg )
batch { a, list } =
    ( a, Cmd.batch list )


map : (a -> b) -> Ret a x -> Ret b x
map func { a, list } =
    fromTuple ( func a, list )


andThen : (a -> Ret b x) -> Ret a x -> Ret b x
andThen func ret =
    let
        { a, list } =
            func ret.a
    in
    fromTuple ( a, ret.list ++ list )


addAll : List x -> Ret a x -> Ret a x
addAll list_ ret =
    { ret | list = list_ ++ ret.list }


add : x -> Ret a x -> Ret a x
add x ret =
    { ret | list = x :: ret.list }


addEffect : (a -> x) -> Ret a x -> Ret a x
addEffect func ret =
    ret |> add (func ret.a)
