module Styles exposing (..)

import Color
import Css
    exposing
        ( auto
        , end
        , inherit
        , int
        , marginBottom
        , marginLeft
        , marginRight
        , marginTop
        , none
        , num
        , padding
        , paddingBottom
        , paddingLeft
        , paddingTop
        , px
        , transparent
        , zero
        )
import Css.Media as Media
import Css.Transitions as CT
import ModularScale


batch =
    Css.batch


type alias Style =
    Css.Style


toCssColor : Color.Color -> Css.Color
toCssColor =
    Color.toHex >> Css.hex


bg : Css.ColorValue compatible -> Css.Style
bg =
    Css.backgroundColor


lh =
    Css.lineHeight << Css.num


bgTransparent : Css.Style
bgTransparent =
    bg transparent


fg =
    Css.color


c_ =
    Css.color


fgInherit =
    fg inherit


grayL : Float -> Css.Color
grayL n =
    Css.hsl 0 0 n


white =
    grayL 1


lightRed =
    Css.hex "#ff725c"


bgGrayL : Float -> Css.Style
bgGrayL =
    bg << grayL


bgWhite : Css.Style
bgWhite =
    bg white


c_white =
    c_ white


c_inherit =
    c_ inherit


pePlaceholder : List Css.Style -> Css.Style
pePlaceholder =
    Css.pseudoElement "placeholder"


bgInherit : Css.Style
bgInherit =
    bg Css.inherit


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


relative =
    Css.position Css.relative


absolute =
    Css.position Css.absolute


h_0 =
    Css.height zero


h_ =
    Css.height << Css.px


h_100 =
    Css.height <| Css.pct 100


min_h_100 =
    Css.minHeight <| Css.pct 100


max_w =
    Css.maxWidth << Css.px


max_w_pct =
    Css.maxWidth << Css.pct


max_vw =
    Css.maxWidth << Css.vw


min_w =
    Css.minWidth << Css.px


min_w_0 =
    min_w 0


center : Css.Style
center =
    Css.batch [ Css.marginLeft Css.auto, Css.marginRight Css.auto ]


paddingHorizontal v =
    Css.batch [ Css.paddingLeft v, Css.paddingRight v ]


paddingVertical v =
    Css.batch [ paddingTop v, paddingBottom v ]


marginHorizontal v =
    Css.batch [ Css.marginLeft v, Css.marginRight v ]


marginVertical v =
    Css.batch [ marginTop v, marginBottom v ]


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


ma =
    Css.margin << sp


ma0 =
    Css.margin zero


ph : Int -> Css.Style
ph =
    paddingHorizontal << sp


pv : Int -> Css.Style
pv =
    paddingVertical << sp


mh : Int -> Css.Style
mh =
    marginHorizontal << sp


mv : Int -> Css.Style
mv =
    marginVertical << sp


pt : Int -> Css.Style
pt =
    paddingTop << sp


pb : Int -> Css.Style
pb =
    paddingBottom << sp


pl : Int -> Css.Style
pl =
    paddingLeft << sp


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


flexColumn =
    Css.flexDirection Css.column


flexRow =
    Css.flexDirection Css.row


flexRowReverse =
    Css.flexDirection Css.rowReverse


itemsCenter =
    Css.alignItems Css.center


justifyCenter =
    Css.justifyContent Css.center


flexGrow1 : Css.Style
flexGrow1 =
    Css.flexGrow (Css.num 1)


flexShrink1 : Css.Style
flexShrink1 =
    Css.flexShrink (Css.num 1)


selfEnd =
    Css.alignSelf end


selfCenter =
    Css.alignSelf Css.center


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


left_0 =
    Css.left zero


right_0 =
    Css.right zero


absFill =
    batch
        [ top_0
        , bottom_0
        , left_0
        , right_0
        ]


left_auto =
    Css.left auto


bottom_0 =
    Css.bottom Css.zero


w_ =
    Css.width << Css.px


bn =
    Css.batch [ Css.borderStyle none, Css.borderWidth zero ]


bo_r : Css.Style
bo_r =
    Css.batch [ Css.borderRightStyle Css.solid, Css.borderRightWidth (Css.px 1) ]


boAll : Css.Style
boAll =
    Css.batch [ Css.borderStyle Css.solid, Css.borderWidth (Css.px 1) ]


bor =
    Css.borderRadius << b_radius


bo_l : Css.Style
bo_l =
    Css.batch [ Css.borderLeftStyle Css.solid, Css.borderLeftWidth (Css.px 1) ]


bo_b : Css.Style
bo_b =
    Css.batch [ Css.borderBottomStyle Css.solid, Css.borderBottomWidth (Css.px 1) ]


bo_t : Css.Style
bo_t =
    Css.batch [ Css.borderTopStyle Css.solid, Css.borderTopWidth (Css.px 1) ]


boColor =
    Css.borderColor


overflowYHidden =
    Css.overflowY Css.hidden


overflowYAuto =
    Css.overflowY Css.auto


autoHideScrollY =
    Css.batch [ overflowYHidden, Css.hover [ overflowYAuto ] ]


min_h_0 =
    Css.minHeight zero


h_auto =
    Css.height auto


min_vh =
    Css.minHeight << Css.vh


vh =
    Css.height << Css.vh


z_ n =
    Css.zIndex <| int n


noSelection =
    Css.property "user-select" "none"


btnReset =
    batch [ noSelection, ma0, pa0, bn, bgInherit, fgInherit, flex, focus [ z_1 ] ]


z_1 =
    z_ 1


linkReset =
    batch
        [ Css.textDecoration Css.none
        , Css.visited [ Css.color Css.inherit ]
        , Css.color Css.inherit
        ]


hidden =
    Css.visibility Css.hidden


visible =
    Css.visibility Css.visible


styleIf bool styles =
    batch
        (if bool then
            styles

         else
            []
        )


pointer =
    Css.cursor Css.pointer


cursorMove =
    Css.cursor Css.move


ttu =
    Css.textTransform Css.uppercase


tal =
    Css.textAlign Css.left


bold =
    Css.fontWeight Css.bold


bolder =
    Css.fontWeight Css.bolder


c_grayL =
    Css.color << grayL


hover =
    Css.hover


tdNone =
    Css.textDecoration Css.none


underline =
    Css.textDecoration Css.underline


focus =
    Css.focus


commonTransitions =
    CT.transition
        [ CT.padding 150
        , CT.margin 150
        , CT.height 150
        , CT.visibility 150
        , CT.transform 150
        ]


focusWithin =
    Css.pseudoClass "focus-within"
