module Appbar exposing (view)

import Emoji
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import MaterialIcons as MI
import Styles exposing (..)
import UI


iBtn styles =
    styled button (btnReset :: styles)


view : List (Html msg)
view =
    [ iBtn [ mr 2 ] [] [ MI.menu ]
    , search
    , UI.filler
    , iBtn [] [] [ MI.add ]
    ]


search =
    styled input
        [ pa 1, br__ 2, bn ]
        [ placeholder <| Emoji.magnifying_glass ++ " Search"
        ]
        []
