module Appbar exposing (view)

import Html.Styled exposing (..)
import MaterialIcons as MI
import Styles exposing (..)
import UI


btnReset =
    batch [ noSelection, ma0, pa0, bn, bgInherit, fgInherit, flex ]


ib styles =
    styled button (btnReset :: styles)


view : List (Html msg)
view =
    [ ib [ mr 4 ] [] [ MI.menu ]
    , UI.search
    , UI.filler
    , ib [] [] [ MI.add ]
    ]
