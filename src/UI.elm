module UI exposing (filler, search)

import Emoji
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)


filler =
    div [ class "flex-grow-1" ] []


search =
    input
        [ class "pa1 br2 bn"
        , placeholder <| Emoji.magnifying_glass ++ " Search"
        ]
        []
