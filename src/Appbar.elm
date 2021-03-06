module Appbar exposing (view)

import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import Styles exposing (..)


iBtn styles =
    styled button (btnReset :: styles)


view : { menuClicked : msg } -> List (Html msg)
view { menuClicked } =
    let
        gap =
            batch [ mr 2, ns [ mr 3 ] ]
    in
    [ iBtn [ selfCenter, dn, ns [ flex ], bgWhite, c_ lightRed, gap, boRad 1 ]
        []
        [ i [ class "material-icons" ] [ text "done_all" ]
        ]
    , menu menuClicked [ gap, ns [ dn ] ]
    , search [ gap, flexShrink1, min_w_0 ]
    , add [ ml_auto ]
    ]


menu menuClicked styles =
    iBtn styles [ onClick menuClicked ] [ i [ class "material-icons" ] [ text "menu" ] ]


add styles =
    iBtn styles [] [ i [ class "material-icons" ] [ text "add" ] ]


search styles =
    div
        [ css
            [ flex
            , relative
            , c_white
            , hover [ bg (Css.rgba 255 255 255 0.2) ]
            , flexGrow1
            , focusWithin [ bgWhite, colorGrayL 0.3 ]
            , batch styles
            , boRad 2
            ]
        ]
        [ div
            [ css [ absolute, left_ 0, top_ 1, z_ 1 ] ]
            [ i [ class "material-icons" ] [ text "search" ] ]
        , styled input
            [ pa 1
            , Css.paddingLeft (Css.px 24)
            , bn
            , boRad 2
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
