module Appbar exposing (view)

import Emoji
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)
import MaterialIcons as MI
import Styles exposing (..)


iBtn styles =
    styled button (btnReset :: styles)


view : { onMenu : msg } -> List (Html msg)
view config =
    [ menu config [ mr 2, ns [ dn ] ]
    , search [ mr 2 ]
    , add [ ml_auto ]
    ]


menu config styles =
    iBtn styles [ onClick config.onMenu ] [ MI.menu ]


add styles =
    iBtn styles [] [ MI.add ]


search styles =
    styled input
        ([ pa 1, br__ 2, bn ] ++ styles)
        [ placeholder <| Emoji.magnifying_glass ++ " Search"
        ]
        []
