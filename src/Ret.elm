module Ret exposing (..)

import Basics.More exposing (msgToCmd)
import Lens exposing (Lens)
import Optional exposing (Optional)
import Return


type alias Ret a x =
    ( a, Cmd x )


only : a -> Ret a x
only a =
    ( a, Cmd.none )


map : (a -> b) -> Ret a x -> Ret b x
map =
    Tuple.mapFirst


andThen : (a -> Ret b x) -> Ret a x -> Ret b x
andThen =
    Return.andThen


addAll : List (Cmd x) -> Ret a x -> Ret a x
addAll list_ =
    Return.command (Cmd.batch list_)


add : Cmd x -> Ret a x -> Ret a x
add =
    Return.command


addMsg : msg -> Ret a msg -> Ret a msg
addMsg msg =
    add (msgToCmd msg)


addEffect : (a -> Cmd x) -> Ret a x -> Ret a x
addEffect =
    Return.effect_


fromElmUpdate : (msg -> a -> Ret a x) -> msg -> RetF a x
fromElmUpdate func msg =
    andThen (func msg)


toElmUpdate : (msg_ -> Ret a msg -> Ret a msg) -> msg_ -> a -> ( a, Cmd msg )
toElmUpdate func msg a =
    only a |> func msg


type alias RetF a x =
    Ret a x -> Ret a x


updateSub : Lens s b -> (msg -> s -> Ret s x) -> msg -> b -> Ret b x
updateSub { get, set } subUpdate msg big =
    subUpdate msg (get big)
        |> map (\small -> set small big)


updateSubF : Lens s b -> (msg -> RetF s x) -> msg -> RetF b x
updateSubF { get, set } subUpdateF msg ( big, bigC ) =
    get big
        |> only
        |> subUpdateF msg
        |> Tuple.mapBoth
            (\small -> set small big)
            (\smallC ->
                Cmd.batch [ bigC, smallC ]
            )


updateOptional : Optional s b -> (msg -> s -> Ret s x) -> msg -> b -> Ret b x
updateOptional { get, set } subUpdate msg big =
    case get big of
        Just small_ ->
            small_
                |> subUpdate msg
                |> Tuple.mapFirst (\small -> set small big)

        Nothing ->
            only big


updateOptionalF : Optional s b -> (msg -> Ret s x -> Ret s x) -> msg -> Ret b x -> Ret b x
updateOptionalF { get, set } subUpdateF msg ( big, bigC ) =
    case get big of
        Just small_ ->
            only small_
                |> subUpdateF msg
                |> Tuple.mapBoth
                    (\small -> set small big)
                    (\smallC ->
                        Cmd.batch [ bigC, smallC ]
                    )

        Nothing ->
            ( big, bigC )


mapSubF : Lens s b -> (s -> s) -> Ret b x -> Ret b x
mapSubF subLens func =
    map (Lens.over subLens func)
