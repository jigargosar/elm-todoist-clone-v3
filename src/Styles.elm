module Styles exposing (..)

import Css
import Css.Media as Media
import ModularScale


bg : Css.ColorValue compatible -> Css.Style
bg =
    Css.backgroundColor


fg =
    Css.color


grayN : Float -> Css.Color
grayN n =
    Css.hsl 0 0 n


white =
    grayN 1


lightRed =
    Css.hex "#ff725c"


bgGrayN : Float -> Css.Style
bgGrayN =
    bg << grayN


bgWhite : Css.Style
bgWhite =
    bg white


bgLightRed =
    bg lightRed


wPct : Float -> Css.Style
wPct =
    Css.width << Css.pct


w_100 =
    wPct 100


top_0 : Css.Style
top_0 =
    Css.top Css.zero


fixed =
    Css.position Css.fixed


h_ =
    Css.height << Css.px


max_w =
    Css.maxWidth << Css.px


center : Css.Style
center =
    Css.batch [ Css.marginLeft Css.auto, Css.marginRight Css.auto ]


paddingHorizontal v =
    Css.batch [ Css.paddingLeft v, Css.paddingRight v ]


modularScaleTachyonsSpacing =
    ModularScale.config [ 0.25 ] ModularScale.Octave


sp : Int -> Float
sp n =
    ModularScale.get modularScaleTachyonsSpacing n


ph : Int -> Css.Style
ph n =
    paddingHorizontal (Css.rem <| sp n)


flex =
    Css.displayFlex


itemsCenter =
    Css.alignItems Css.center



--flexGrow1 : Css.Style
--flexGrow1 =
--    Css.flexGrow (Css.num 1)


dn : Css.Style
dn =
    Css.display Css.none


db : Css.Style
db =
    Css.display Css.block


ns : List Css.Style -> Css.Style
ns =
    Media.withMedia [ Media.only Media.screen [ Media.minWidth <| Css.rem 30 ] ]


topPx =
    Css.top << Css.px


bottom_0 =
    Css.bottom Css.zero


widthPx =
    Css.width << Css.px


br_ : Css.Style
br_ =
    Css.batch [ Css.borderRightStyle Css.solid, Css.borderRightWidth (Css.px 1) ]


bl : Css.Style
bl =
    Css.batch [ Css.borderLeftStyle Css.solid, Css.borderLeftWidth (Css.px 1) ]


b__ =
    Css.borderColor


overflowYHidden =
    Css.overflowY Css.hidden


overflowYAuto =
    Css.overflowY Css.auto


autoHideScrollY =
    Css.batch [ overflowYHidden, Css.hover [ overflowYAuto ] ]


min_vh =
    Css.minHeight << Css.vh


vh =
    Css.height << Css.vh
