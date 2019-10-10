module Layout exposing (Parts, view)

import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)


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


bgGrayN : Float -> Css.Style
bgGrayN =
    bg << grayN


bgWhite : Css.Style
bgWhite =
    bg white



-- Custom Styles


bgBody : Css.Style
bgBody =
    bgGrayN 0.98


fgWhite =
    fg white


view : Parts msg -> Html msg
view { top, side, main } =
    styled div
        [ bgBody ]
        []
        [ styled header
            [ fgWhite ]
            [ class "fixed top-0 bg-light-red white w-100 h-header" ]
            [ div
                ([ class "center w-100 max-w-app ph2" ]
                    ++ [ class "h-100", class "flex items-center" ]
                )
                top
            ]
        , div [ class "center w-100 max-w-app ", class "flex-grow-1" ]
            [ aside
                [ class "dn db-ns fixed top-sidebar bottom-0 w-sidebar hover-overflow-y  br-ns b--main"
                ]
                side
            , styled div
                [ bgWhite ]
                [ class "ml0 ml-main-ns pt-main min-vh-100 flex-grow-1 flex"
                ]
                [ main_ [ class "flex-grow-1 br-ns b--main" ] main ]
            ]
        ]
