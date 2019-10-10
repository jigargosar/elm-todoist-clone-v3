module Layout exposing (Parts, view)

import Css exposing (batch)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Styles exposing (..)


type alias Parts msg =
    { top : List (Html msg)
    , side : List (Html msg)
    , main : List (Html msg)
    }



-- Custom Styles


bgBody : Css.Style
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
        [ styled header
            [ fgWhite, bgLightRed, fixed, top_0, w_100, h_header, flex ]
            []
            [ styled div
                ([ center, w_100, max_w maxAppWidthPx ]
                    ++ [ ph 2, flex, itemsCenter ]
                )
                []
                top
            ]
        , styled div
            [ center, w_100, max_w maxAppWidthPx ]
            []
            [ styled aside
                [ Css.batch [ dn, ns [ db ] ]
                , Css.batch [ fixed, top_ headerHeightPx, bottom_0, w_sidebar ]
                , autoHideScrollY
                ]
                []
                --                [ styled div [ Css.height (Css.vh 200) ] [] side ] -- TEST OVERFLOW SCROLL
                side
            , styled div
                [ batch [ ml0, ns [ ml_ sidebarWidthPx ] ]
                , pt_ headerHeightPx
                , min_vh 100
                , bgWhite
                , ns [ br_, bl, b__main ]
                , flex
                ]
                [ class "pt-main "
                ]
                [ main_ [ class "flex-grow-1" ] main ]
            ]
        ]
