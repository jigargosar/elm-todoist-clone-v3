module UI exposing (addIconBtn, filler, search)

import Emoji
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)


filler =
    div [ class "flex-grow-1" ] []


addIconBtn =
    Emoji.buttonNoMsg Emoji.heavy_plus_sign


search =
    input
        [ class "pa1 br2 bn"
        , placeholder <| Emoji.magnifying_glass ++ " Search"
        ]
        []
