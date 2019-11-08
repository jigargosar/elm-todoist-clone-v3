module Ret exposing (..)


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


type alias Lens s b =
    { get : b -> s, set : s -> b -> b }


createLens : ( b -> s, s -> b -> b ) -> Lens s b
createLens ( get, set ) =
    { get = get, set = set }


over : Lens s b -> (s -> s) -> b -> b
over lens func val =
    lens.set (func <| lens.get val) val


liftUpdate :
    (a
     -> ( a, Cmd msg )
    )
    -> (RetCmd a msg -> RetCmd a msg)
liftUpdate func retCmd =
    let
        ( a, cmd ) =
            func retCmd.a
    in
    only a |> addAll retCmd.list |> add cmd


toElmUpdate : (RetCmd a msg -> RetCmd a msg) -> (a -> ( a, Cmd msg ))
toElmUpdate func a =
    only a
        |> func
        |> batch
