module Emoji exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


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
button msg emoji =
    Html.button [ class "select-none pa2 bn bg-inherit color-inherit", onClick msg ]
        [ view emoji ]
