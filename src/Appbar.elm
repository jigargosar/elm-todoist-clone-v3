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
    let
        gap =
            batch [ mr 2, ns [ mr 3 ] ]
    in
    [ iBtn [ selfCenter, dn, ns [ flex ], bgWhite, c_ lightRed, gap, bor 1 ] [] [ MI.done_all ]
    , menu config [ gap, ns [ dn ] ]
    , search [ gap, flexShrink1, min_w_0 ]
    , add [ ml_auto ]
    ]


menu config styles =
    iBtn styles [ onClick config.onMenu ] [ MI.menu ]


add styles =
    iBtn styles [] [ MI.add ]


search styles =
    styled input
        ([ pa 1, bor 2, bn ] ++ styles)
        [ placeholder <| Emoji.magnifying_glass ++ " Search"
        ]
        []
