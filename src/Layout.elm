module Layout exposing (Parts, view)

import Css
import Css.Media as Media
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import ModularScale


type alias Parts msg =
    { top : List (Html msg)
    , side : List (Html msg)
    , main : List (Html msg)
    }


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


height =
    Css.height << Css.px


maxWidth =
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


width =
    Css.width << Css.px


br : Css.Style
br =
    Css.batch [ Css.borderRightStyle Css.solid, Css.borderRightWidth (Css.px 1) ]


bl : Css.Style
bl =
    Css.batch [ Css.borderLeftStyle Css.solid, Css.borderLeftWidth (Css.px 1) ]


bc =
    Css.borderColor


overflowYHidden =
    Css.overflowY Css.hidden


overflowYAuto =
    Css.overflowY Css.auto


autoHideScrollY =
    Css.batch [ overflowYHidden, Css.hover [ overflowYAuto ] ]



-- Custom Styles


bgBody : Css.Style
bgBody =
    bgGrayN 0.98


fgWhite =
    fg white


headerHeightPx =
    50


maxAppWidthPx =
    922


sidebarWidthPx =
    266


bc_main =
    bc (grayN 0.95)


view : Parts msg -> Html msg
view { top, side, main } =
    styled div
        [ bgBody ]
        []
        [ styled header
            [ fgWhite, bgLightRed, fixed, top_0, w_100, height headerHeightPx, flex ]
            []
            [ styled div
                ([ center, w_100, maxWidth maxAppWidthPx ]
                    ++ [ ph 2, flex, itemsCenter ]
                )
                []
                top
            ]
        , styled div
            [ center, w_100, maxWidth maxAppWidthPx ]
            []
            [ styled aside
                [ Css.batch [ dn, ns [ db ] ]
                , Css.batch [ fixed, topPx headerHeightPx, bottom_0, width sidebarWidthPx ]
                , autoHideScrollY
                ]
                []
                --                [ styled div [ Css.height (Css.vh 200) ] [] side ] -- TEST OVERFLOW SCROLL
                side
            , styled div
                [ bgWhite, ns [ br, bl, bc_main ], flex, Css.minHeight (Css.vh 100) ]
                [ class "ml0 ml-main-ns pt-main "
                ]
                [ main_ [ class "flex-grow-1" ] main ]
            ]
        ]
