module Ret exposing (..)

import Basics.More exposing (msgToCmd)
import Lens exposing (Lens)
import Optional exposing (Optional)
import Return


type alias Ret a x =
    ( a, Cmd x )


type alias RetCmd a msg =
    Ret a msg


only : a -> Ret a x
only a =
    ( a, Cmd.none )


fromTuple =
    identity


fromElmTuple =
    identity


batch =
    identity


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


addMsg : msg -> RetCmd a msg -> RetCmd a msg
addMsg msg =
    add (msgToCmd msg)


addEffect : (a -> Cmd x) -> Ret a x -> Ret a x
addEffect =
    Return.effect_


liftElmUpdate : (b -> a -> ( c, Cmd msg )) -> b -> RetCmd a msg -> RetCmd c msg
liftElmUpdate func msg =
    andThen (func msg)


toElmUpdate : (msg_ -> RetCmd a msg -> RetCmd a msg) -> msg_ -> a -> ( a, Cmd msg )
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
    let
        ( small, smallC ) =
            toElmUpdate subUpdateF msg (get big)
    in
    ( set small big, Cmd.batch [ bigC, smallC ] )


updateOptional : Optional s b -> (msg -> RetCmd s x -> RetCmd s x) -> msg -> RetCmd b x -> RetCmd b x
updateOptional optional subUpdate msg ( big, bigC ) =
    case optional.get big of
        Just small_ ->
            let
                ( small, smallC ) =
                    subUpdate msg (only small_)
            in
            ( optional.set small big, Cmd.batch [ bigC, smallC ] )

        Nothing ->
            ( big, bigC )


mapSub : Lens s b -> (s -> s) -> Ret b x -> Ret b x
mapSub subLens func =
    map (Lens.over subLens func)
