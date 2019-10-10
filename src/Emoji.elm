module Emoji exposing (..)

import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)


heavy_plus_sign =
    "➕"


magnifying_glass =
    "🔍"


heavy_large_circle =
    "⭕"


heavy_check_mark =
    "✔"


view : String -> Html msg
view emoji =
    span [ class "dib lh-solid", style "width" "1em" ] [ text emoji ]


button : msg -> String -> Html msg
button msg =
    buttonHelp (Just msg)


buttonHelp : Maybe msg -> String -> Html msg
buttonHelp maybeMsg emoji =
    Html.button
        [ class "select-none pa2 bn bg-inherit color-inherit"
        , Maybe.map onClick maybeMsg
            |> Maybe.withDefault (class "")
        ]
        [ view emoji ]


buttonNoMsg : String -> Html msg
buttonNoMsg =
    buttonHelp Nothing
