module UI exposing (..)

import Emoji
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


filler =
    div [ class "flex-grow-1" ] []


topBar c =
    div [ class "absolute top-0 min-w-100 ph2 pv1 bg-black white bn shadow-1" ]
        [ div [ class "measure center flex" ] c ]


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
