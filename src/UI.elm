module UI exposing (addIconBtn, filler, search)

import Emoji
import Html exposing (..)
import Html.Attributes exposing (..)


filler =
    div [ class "flex-grow-1" ] []


addIconBtn =
    button [ class "pa1 lh-solid bn bg-inherit color-inherit" ]
        [ text Emoji.heavy_plus_sign
        ]


search =
    input
        [ class "pa1 br2 bn"
        , placeholder <| Emoji.magnifying_glass ++ " Search"
        ]
        []
