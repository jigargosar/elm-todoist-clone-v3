module Appbar exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import UI


view : Html msg
view =
    div []
        [ UI.topBar [ UI.search, UI.filler, UI.addIconBtn ]
        , div [ style "height" "2.5rem" ] []
        ]
