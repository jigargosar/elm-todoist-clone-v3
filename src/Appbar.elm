module Appbar exposing (view)

import Html.Styled exposing (..)
import MaterialIcons
import UI


view : List (Html msg)
view =
    [ UI.search
    , UI.filler
    , MaterialIcons.add
    , MaterialIcons.buttonHelp
        Nothing
        [ MaterialIcons.search ]
    , UI.addIconBtn
    ]
