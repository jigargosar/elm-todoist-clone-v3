module Key exposing (escape, onKeyDown)

import Css
import FilterId exposing (FilterId)
import Html.Styled exposing (Attribute, Html, button, div, input, label, span, text)
import Html.Styled.Attributes as A exposing (css, type_, value)
import Html.Styled.Events as E exposing (onClick)
import Json.Decode as JD
import LabelId exposing (LabelId)
import ProjectId exposing (ProjectId)
import Styles exposing (..)
import Theme
import View exposing (View)


escape : a -> JD.Decoder a
escape =
    keyEq "Escape"


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
