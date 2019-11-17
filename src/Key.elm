module Key exposing (arrowDown, arrowUp, enter, enterOrSpace, escape, onKeyDown, onKeyDownCustom, preventDefaultOnKeyDown, space, stopBoth, stopPropagationOnKeyDown)

import Basics.More exposing (apply)
import Html.Styled exposing (Attribute, Html)
import Html.Styled.Events as E
import Json.Decode as JD


escape : a -> JD.Decoder a
escape =
    keyEq "Escape"


enter : a -> JD.Decoder a
enter =
    keyEq "Enter"


space : a -> JD.Decoder a
space =
    keyEq " "


arrowDown : a -> JD.Decoder a
arrowDown =
    keyEq "ArrowDown"


arrowUp : a -> JD.Decoder a
arrowUp =
    keyEq "ArrowUp"


enterOrSpace : a -> JD.Decoder a
enterOrSpace msg =
    [ enter, space ] |> List.map (apply msg) |> JD.oneOf


keyEq : String -> a -> JD.Decoder a
keyEq expectedKey msg =
    JD.field "key" JD.string
        |> JD.andThen
            (\key ->
                if key == expectedKey then
                    JD.succeed msg

                else
                    JD.fail "no match"
            )


onKeyDown : List (JD.Decoder msg) -> Attribute msg
onKeyDown decoders =
    E.on "keydown" (JD.oneOf decoders)


preventDefaultOnKeyDown : List (JD.Decoder ( msg, Bool )) -> Attribute msg
preventDefaultOnKeyDown decoders =
    E.preventDefaultOn "keydown" (JD.oneOf decoders)


stopPropagationOnKeyDown : List (JD.Decoder ( msg, Bool )) -> Attribute msg
stopPropagationOnKeyDown decoders =
    E.stopPropagationOn "keydown" (JD.oneOf decoders)


onKeyDownCustom : List (JD.Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool }) -> Attribute msg
onKeyDownCustom decoders =
    E.custom "keydown" (JD.oneOf decoders)


stopBoth : a -> { message : a, stopPropagation : Bool, preventDefault : Bool }
stopBoth msg =
    { message = msg, stopPropagation = True, preventDefault = True }
