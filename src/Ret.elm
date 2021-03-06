module Ret exposing (..)

import Basics.More exposing (flip, msgToCmd)
import Lens exposing (Lens)
import Optional exposing (Optional)
import Return
import Task
import Time


type alias Ret a x =
    ( a, Cmd x )


type alias MaybeRet a x =
    Maybe (Ret a x)


only : a -> Ret a x
only a =
    ( a, Cmd.none )


map : (a -> b) -> Ret a x -> Ret b x
map =
    Tuple.mapFirst


andThenIgnoreNothing : (a -> Maybe (Ret b x)) -> Ret b x -> Ret a x -> Ret b x
andThenIgnoreNothing func ret =
    andThen (func >> Maybe.withDefault ret)


mapBoth =
    flip Return.mapBoth


always : a -> Ret b x -> Ret a x
always a =
    map (Basics.always a)


andThen : (a -> Ret b x) -> Ret a x -> Ret b x
andThen =
    Return.andThen


flatten : Ret (Ret a x) x -> Ret a x
flatten =
    andThen identity


andThenF : (a -> RetF a x) -> RetF a x
andThenF func =
    andThen (\m -> func m (only m))


filterWith : (a -> Maybe b) -> (b -> a -> a) -> RetF a x
filterWith func func2 ret =
    case Tuple.first ret |> func of
        Just b ->
            ret |> map (func2 b)

        Nothing ->
            ret


andThenFilter : (a -> MaybeRet a x) -> RetF a x
andThenFilter func (( m, c ) as ret) =
    case func m of
        Just ( m2, c2 ) ->
            ( m2, Cmd.batch [ c, c2 ] )

        Nothing ->
            ret


andThenFilterWith : (a -> Maybe s) -> (s -> a -> Ret a x) -> RetF a x
andThenFilterWith func1 func2 ret =
    case Tuple.first ret |> func1 of
        Just s ->
            ret |> andThen (func2 s)

        Nothing ->
            ret


andThenFilterWithF : (a -> Maybe s) -> (s -> RetF a x) -> RetF a x
andThenFilterWithF func1 func2 ret =
    case Tuple.first ret |> func1 of
        Just s ->
            func2 s ret

        Nothing ->
            ret


andThenAlways : Ret b x -> Ret a x -> Ret b x
andThenAlways ret =
    andThen (Basics.always ret)


addAll : List (Cmd x) -> RetF a x
addAll list_ =
    Return.command (Cmd.batch list_)


add : Cmd x -> RetF a x
add =
    Return.command


perform : Task.Task Never a -> (a -> msg) -> Ret b msg -> Ret b msg
perform task toMsg =
    add (Task.perform toMsg task)


getNow : (Time.Posix -> msg) -> Ret a msg -> Ret a msg
getNow toMsg =
    add (Task.perform toMsg Time.now)


addMsg : msg -> RetF a msg
addMsg msg =
    add (msgToCmd msg)


toCmd =
    msgToCmd


addEffect : (a -> Cmd x) -> Ret a x -> Ret a x
addEffect =
    Return.effect_


addMsgEffect : (a -> x) -> Ret a x -> Ret a x
addMsgEffect func =
    Return.effect_ (func >> msgToCmd)


toUpdateF : (msg -> a -> Ret a x) -> msg -> RetF a x
toUpdateF func msg =
    andThen (func msg)


fromUpdateF : (msg_ -> Ret a msg -> Ret a msg) -> msg_ -> a -> ( a, Cmd msg )
fromUpdateF func msg a =
    only a |> func msg


type alias RetF a x =
    Ret a x -> Ret a x


updateSub : Lens s b -> (msg -> s -> Ret s x) -> msg -> b -> Ret b x
updateSub { get, set } subUpdate msg big =
    subUpdate msg (get big)
        |> map (\small -> set small big)


mapCmd =
    Return.mapCmd


mapSubF : Lens s b -> (s -> s) -> RetF b x
mapSubF subLens func =
    map (Lens.over subLens func)


setSub : Lens s b -> s -> RetF b x
setSub { set } small =
    map (set small)


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


type alias Setter s b =
    s -> b -> b


type alias HasSetter x s b =
    { x
        | set : Setter s b
    }


initSub : HasSetter xx s b -> ( s, Cmd x ) -> RetF b x
initSub { set } ( sub, subC ) =
    andThen (\big -> ( set sub big, subC ))


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


addError : (error -> Cmd x) -> Result error value -> Ret a x -> Ret a x
addError func result =
    case result of
        Err error ->
            add (func error)

        Ok _ ->
            identity
