module Appbar exposing (view)

import Html.Styled exposing (..)
import MaterialIcons
import Styles exposing (ph)
import UI


view : List (Html msg)
view =
    [ UI.menuIconBtn
    , styled div [ ph 1 ] [] []
    , UI.search
    , UI.filler
    , UI.addIconBtn
    ]
