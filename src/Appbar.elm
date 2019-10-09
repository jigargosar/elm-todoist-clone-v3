module Appbar exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import UI


view : List (Html msg)
view =
    [ UI.search, UI.filler, UI.addIconBtn ]
