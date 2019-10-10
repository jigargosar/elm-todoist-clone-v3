module Styles exposing (..)

import Css exposing (auto, end, inherit, int, marginLeft, marginRight, none, num, padding, paddingTop, px, zero)
import Css.Media as Media
import ModularScale


batch =
    Css.batch


type alias Style =
    Css.Style


bg : Css.ColorValue compatible -> Css.Style
bg =
    Css.backgroundColor


bgInherit =
    bg inherit


fg =
    Css.color


fgInherit =
    fg inherit


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


sp n =
    if n <= 0 then
        Css.rem 0

    else
        ModularScale.get modularScaleTachyonsSpacing (n - 1) |> Css.rem


b_radius n =
    if n <= 0 then
        Css.rem 0

    else
        ModularScale.get (ModularScale.config [ 0.125 ] ModularScale.Octave) (n - 1)
            |> Css.rem


pa0 =
    Css.padding zero


pa =
    padding << sp


ma0 =
    Css.margin zero


ph : Int -> Css.Style
ph =
    paddingHorizontal << sp


pt : Int -> Css.Style
pt =
    paddingTop << sp


ml =
    marginLeft << sp


ml_auto =
    marginLeft auto


mr =
    marginRight << sp


ml_ : Float -> Css.Style
ml_ =
    marginLeft << px


pt_ : Float -> Css.Style
pt_ =
    paddingTop << px


ml0 =
    marginLeft zero


flex =
    Css.displayFlex


itemsCenter =
    Css.alignItems Css.center


flexGrow1 : Css.Style
flexGrow1 =
    Css.flexGrow (Css.num 1)


selfEnd =
    Css.alignSelf end


dn : Css.Style
dn =
    Css.display Css.none


db : Css.Style
db =
    Css.display Css.block


ns : List Css.Style -> Css.Style
ns =
    Media.withMedia [ Media.only Media.screen [ Media.minWidth <| Css.rem 30 ] ]


top_ =
    Css.top << Css.px


left_ =
    Css.left << Css.px


bottom_0 =
    Css.bottom Css.zero


w_ =
    Css.width << Css.px


bn =
    Css.batch [ Css.borderRightStyle none, Css.borderWidth zero ]


br_ : Css.Style
br_ =
    Css.batch [ Css.borderRightStyle Css.solid, Css.borderRightWidth (Css.px 1) ]


br__ =
    Css.borderRadius << b_radius


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


z_ n =
    Css.zIndex <| int n


noSelection =
    Css.property "user-select" "none"


btnReset =
    batch [ noSelection, ma0, pa0, bn, bgInherit, fgInherit, flex ]
