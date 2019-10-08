module Appbar exposing (view)

import Html exposing (Html)
import UI


view : Html msg
view =
    UI.topBar [ UI.search, UI.filler, UI.addIconBtn ]
