module Appbar exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import UI


view : List (Attribute msg) -> Html msg
view attrs =
    div (attrs ++ [ class "flex items-center w-100 ph3" ]) [ UI.search, UI.filler, UI.addIconBtn ]
