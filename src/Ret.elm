module Ret exposing (..)

import Lens exposing (Lens)


type alias Ret a x =
    { a : a, list : List x }


type alias RetCmd a msg =
    Ret a (Cmd msg)


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


liftUpdate : (b -> a -> ( c, d )) -> b -> Ret a d -> Ret c d
liftUpdate func msg retCmd =
    let
        ( a, cmd ) =
            func msg retCmd.a
    in
    only a |> addAll retCmd.list |> add cmd


toElmUpdate : (msg_ -> RetCmd a msg -> RetCmd a msg) -> msg_ -> a -> ( a, Cmd msg )
toElmUpdate func msg a =
    only a
        |> func msg
        |> batch


updateSub : Lens s b -> (msg -> Ret s x -> Ret s x) -> msg -> Ret b x -> Ret b x
updateSub subLens subUpdate msg ret =
    let
        subRet =
            subUpdate msg (only (subLens.get ret.a))
    in
    fromTuple ( subLens.set subRet.a ret.a, ret.list ++ subRet.list )


mapSub : Lens s b -> (s -> s) -> Ret b x -> Ret b x
mapSub subLens func =
    map (Lens.over subLens func)



-- Lens
