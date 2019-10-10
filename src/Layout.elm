module Layout exposing (Parts, view)

import Css
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


view : Parts msg -> Html msg
view { top, side, main } =
    styled div
        [ bgBody ]
        []
        [ styled header
            [ fgWhite, bgLightRed, fixed, top_0, w_100, h_header, flex ]
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
                , Css.batch [ fixed, topPx headerHeightPx, bottom_0, widthPx sidebarWidthPx ]
                , autoHideScrollY
                ]
                []
                --                [ styled div [ Css.height (Css.vh 200) ] [] side ] -- TEST OVERFLOW SCROLL
                side
            , styled div
                [ bgWhite, ns [ br_, bl, b__main ], flex, min_vh 100 ]
                [ class "ml0 ml-main-ns pt-main "
                ]
                [ main_ [ class "flex-grow-1" ] main ]
            ]
        ]
