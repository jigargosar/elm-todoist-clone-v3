module UI exposing (addIconBtn, filler, menuIconBtn, search)

import Emoji
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import MaterialIcons as MI


filler =
    div [ class "flex-grow-1" ] []


addIconBtn : Html msg
addIconBtn =
    MI.buttonHelp Nothing [ MI.add ]


menuIconBtn : Html msg
menuIconBtn =
    MI.buttonHelp Nothing [ MI.menu ]


search =
    input
        [ class "pa1 br2 bn"
        , placeholder <| Emoji.magnifying_glass ++ " Search"
        ]
        []
