module Css.More exposing
    ( appearanceNone
    , backgroundColor
    , backgroundColorHSL
    , backgroundColorWhite
    , borderColor
    , borderColorHSL
    , borderColorTransparent
    , color
    , colorHSL
    , colorWhite
    , fromColorWithAlpha
    , textDecorationNone
    , transitionWithDelay
    , userSelectNone
    )

import Color
import Color.Transparent
import Css
import Css.Transitions as T


fromColor : Color.Color -> Css.Color
fromColor color_ =
    let
        ( r, g, b ) =
            Color.toRGB color_
    in
    Css.rgb (round r) (round g) (round b)


fromTransparentColor : Color.Transparent.Color -> Css.Color
fromTransparentColor tColor =
    let
        { red, green, blue, alpha } =
            Color.Transparent.toRGBA tColor
    in
    Css.rgba (round red) (round green) (round blue) (Color.Transparent.opacityToFloat alpha)


fromColorWithAlpha : Color.Color -> Float -> Css.Color
fromColorWithAlpha color_ =
    let
        ( r, g, b ) =
            Color.toRGB color_
    in
    Css.rgba (round r) (round g) (round b)


color : Color.Color -> Css.Style
color =
    Css.color << fromColor


backgroundColor : Color.Color -> Css.Style
backgroundColor =
    Css.backgroundColor << fromColor


borderColor : Color.Color -> Css.Style
borderColor =
    Css.borderColor << fromColor


borderColorTransparent : Css.Style
borderColorTransparent =
    Css.borderColor Css.transparent


colorWhite : Css.Style
colorWhite =
    Css.color white


backgroundColorWhite : Css.Style
backgroundColorWhite =
    Css.backgroundColor white


hslProperty : (Css.Color -> Css.Style) -> Float -> Float -> Float -> Css.Style
hslProperty propFn h s l =
    propFn (Css.hsl h s l)


colorHSL : Float -> Float -> Float -> Css.Style
colorHSL =
    hslProperty Css.color


backgroundColorHSL : Float -> Float -> Float -> Css.Style
backgroundColorHSL =
    hslProperty Css.backgroundColor


borderColorHSL : Float -> Float -> Float -> Css.Style
borderColorHSL =
    hslProperty Css.borderColor


white =
    Css.rgb 255 255 255


transitionWithDelay : a -> List (a -> T.Transition) -> Css.Style
transitionWithDelay delay =
    List.map (\t -> t delay) >> T.transition


textDecorationNone : Css.Style
textDecorationNone =
    Css.textDecoration Css.none


prefixes =
    [ "-webkit-", "-moz-", "-ms-" ]


prefixedProperty name value =
    prefixes
        |> List.map (\prefix -> Css.property (prefix ++ name) value)
        |> Css.batch


appearanceNone : Css.Style
appearanceNone =
    prefixedProperty "appearance" "none"


userSelectNone : Css.Style
userSelectNone =
    prefixedProperty "user-select" "none"
