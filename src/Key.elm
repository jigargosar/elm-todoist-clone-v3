module Key exposing (enter, escape, onKeyDown, space)

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
