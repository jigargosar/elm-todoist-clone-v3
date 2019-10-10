module UI exposing (addIconBtn, filler, search)

import Emoji
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import MaterialIcons


filler =
    div [ class "flex-grow-1" ] []


addIconBtn : Html msg
addIconBtn =
    MaterialIcons.buttonHelp Nothing [ MaterialIcons.add ]


search =
    input
        [ class "pa1 br2 bn"
        , placeholder <| Emoji.magnifying_glass ++ " Search"
        ]
        []
