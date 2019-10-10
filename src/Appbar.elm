module Appbar exposing (view)

import Html.Styled exposing (..)
import MaterialIcons
import UI


view : List (Html msg)
view =
    [ UI.search
    , UI.filler
    , UI.addIconBtn
    ]
