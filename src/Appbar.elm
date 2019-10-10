module Appbar exposing (view)

import Emoji
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import MaterialIcons as MI
import Styles exposing (..)
import UI


ib styles =
    styled button (btnReset :: styles)


view : List (Html msg)
view =
    [ ib [ mr 4 ] [] [ MI.menu ]
    , search
    , UI.filler
    , ib [] [] [ MI.add ]
    ]


search =
    input
        [ class "pa1 br2 bn"
        , placeholder <| Emoji.magnifying_glass ++ " Search"
        ]
        []
