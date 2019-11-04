module Css.More exposing (..)

import Color
import Color.Transparent
import Css


fromColor : Color.Color -> Css.Color
fromColor color_ =
    let
        ( r, g, b ) =
            Color.toRGB color_
    in
    Css.rgb (round r) (round g) (round b)


fromTransparentColor tColor =
    let
        { red, green, blue, alpha } =
            Color.Transparent.toRGBA tColor
    in
    Css.rgba (round red) (round green) (round blue) (Color.Transparent.opacityToFloat alpha)
