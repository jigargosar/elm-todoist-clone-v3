module UI.Modal exposing (container, overlay)

import Css exposing (..)
import Html.Styled exposing (Attribute, Html, div)
import Html.Styled.Attributes exposing (class, css)


container : List (Html msg) -> Html msg
container =
    div [ class "modal is-active" ]


overlay : List (Attribute msg) -> Html msg
overlay attrs =
    div
        (css [ backgroundColor (hsla 0 0 0 0.2) ]
            :: class "modal-background"
            :: attrs
        )
        []
