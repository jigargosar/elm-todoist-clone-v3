module Appbar exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import UI


view : List (Html msg)
view =
    [ div [ class "flex items-center w-100 ph3" ] [ UI.search, UI.filler, UI.addIconBtn ] ]
