module Theme exposing (..)

import Color
import Color.Transparent
import Css exposing (Color, hex, hsl, rgb)
import Styles


borderGray : Color
borderGray =
    Styles.grayL 0.8


white =
    rgb 255 255 255



{- primary =
   hsl 241 0.63 0.59
-}


primary =
    Color.fromHSL ( 241, 63, 60 )


colorToCssColor : Color.Color -> Css.Color
colorToCssColor color_ =
    let
        ( r, g, b ) =
            Color.toRGB color_
    in
    Css.rgb (round r) (round g) (round b)


transparentColorToCssColor : Color.Transparent.Color -> Css.Color
transparentColorToCssColor tColor =
    let
        { red, green, blue, alpha } =
            Color.Transparent.toRGBA tColor
    in
    Css.rgba (round red) (round green) (round blue) (Color.Transparent.opacityToFloat alpha)
