module Theme exposing (..)

import Color
import Color.Transparent
import Css exposing (Color, hex, hsl, rgb)
import Styles


borderGray : Color
borderGray =
    Styles.grayL 0.8


white : Color
white =
    rgb 255 255 255



{- primary =
   hsl 241 0.63 0.59
-}


primary : Color.Color
primary =
    Color.fromHSL ( 241, 63, 59 )


toCssColor : Color.Color -> Css.Color
toCssColor color_ =
    let
        ( r, g, b ) =
            Color.toRGB color_
    in
    Css.rgb (round r) (round g) (round b)


toCssColorAlpha : Color.Transparent.Color -> Css.Color
toCssColorAlpha tColor =
    let
        { red, green, blue, alpha } =
            Color.Transparent.toRGBA tColor
    in
    Css.rgba (round red) (round green) (round blue) (Color.Transparent.opacityToFloat alpha)


primaryWhiten n =
    primary |> Color.whiten n


primaryBlacken n =
    primary |> Color.blacken n


primaryAlpha n =
    Color.Transparent.fromColor (Color.Transparent.customOpacity n) primary
