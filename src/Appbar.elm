module Appbar exposing (view)

import Css
import Emoji
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import MaterialIcons as MI
import Styles exposing (..)


iBtn styles =
    styled button (btnReset :: styles)


view : { onMenu : msg } -> List (Html msg)
view config =
    let
        gap =
            batch [ mr 2, ns [ mr 3 ] ]
    in
    [ iBtn [ selfCenter, dn, ns [ flex ], bgWhite, c_ lightRed, gap, bor 1 ] [] [ MI.done_all ]
    , menu config [ gap, ns [ dn ] ]
    , search [ gap, flexShrink1, min_w_0 ]
    , add [ ml_auto ]
    ]


menu config styles =
    iBtn styles [ onClick config.onMenu ] [ MI.menu ]


add styles =
    iBtn styles [] [ MI.add ]


search styles =
    div
        [ css
            [ flex
            , relative
            , c_white
            , hover [ bg (Css.rgba 255 255 255 0.2) ]
            , focusWithin [ bgWhite, c_grayL 0.3, flexGrow1 ]
            , batch styles
            , bor 2
            ]
        ]
        [ div
            [ css [ absolute, left_ 0, top_ 1, z_ 1 ] ]
            [ MI.search ]
        , styled input
            [ pa 1
            , Css.paddingLeft (Css.px 24)
            , bn
            , bor 2
            , min_w_0
            , bgTransparent
            , c_inherit
            , pePlaceholder [ c_inherit, Css.fontWeight <| Css.int 100 ]
            , flexGrow1
            , Css.width (Css.em 12)
            ]
            [ placeholder <| " Search"
            ]
            []
        ]
