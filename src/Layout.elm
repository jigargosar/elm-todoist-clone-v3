module Layout exposing (Parts, view)

import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import Styles exposing (..)


type alias Parts msg =
    { top : List (Html msg)
    , side : List (Html msg)
    , main : List (Html msg)
    }



-- Custom Styles


bgBody : Style
bgBody =
    bgGrayN 0.98


fgWhite =
    fg white


b__main =
    b__ (grayN 0.95)


headerHeightPx =
    50


maxAppWidthPx =
    922


sidebarWidthPx =
    266


h_header =
    h_ headerHeightPx


w_sidebar =
    w_ sidebarWidthPx


view : Parts msg -> Html msg
view { top, side, main } =
    styled div
        [ bgBody ]
        []
        [ styled div
            [ fgWhite
            , bgLightRed
            , batch [ fixed, top_0, w_100, h_header ]
            , flex
            , z_ 10
            ]
            []
            [ styled header
                [ batch [ center, w_100, max_w maxAppWidthPx ]
                , batch [ ph 2, flex, itemsCenter ]
                ]
                []
                top
            ]
        , styled div
            [ center, w_100, max_w maxAppWidthPx ]
            []
            [ styled aside
                [ batch [ left_ -sidebarWidthPx, ns [ left_ 0 ] ]
                , transition [ Transitions.left 200 ]
                , batch [ fixed, top_0, bottom_0, pt_ headerHeightPx, w_sidebar ]
                , autoHideScrollY
                ]
                []
                --                [ styled div [ Css.height (Css.vh 200) ] [] side ] -- TEST OVERFLOW SCROLL
                side
            , styled div
                [ batch [ ml0, ns [ ml_ sidebarWidthPx ], transition [ Transitions.marginLeft 200 ] ]
                , pt_ headerHeightPx
                , min_vh 100
                , bgWhite
                , ns [ br_, bl, b__main ]
                , flex
                ]
                []
                [ styled main_ [ flexGrow1 ] [] main ]
            ]
        ]
