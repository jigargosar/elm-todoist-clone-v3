module Emoji exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


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
