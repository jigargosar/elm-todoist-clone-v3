module UI.Modal exposing (..)

import Css exposing (..)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (class, css)


view : List (Html msg) -> Html msg
view content =
    div [ class "modal is-active" ]
        [ div [ css [ backgroundColor (hsla 0 0 0 0.2) ], class "modal-background" ] []
        , div [ class "modal-content" ] content
        ]
