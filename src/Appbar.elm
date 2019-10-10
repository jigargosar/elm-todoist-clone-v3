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
    [ menu [ mr 2 ]
    , search [ mr 2 ]
    , add [ ml_auto ]
    ]


menu styles =
    iBtn styles [] [ MI.menu ]


add styles =
    iBtn styles [] [ MI.add ]


search styles =
    styled input
        ([ pa 1, br__ 2, bn ] ++ styles)
        [ placeholder <| Emoji.magnifying_glass ++ " Search"
        ]
        []
