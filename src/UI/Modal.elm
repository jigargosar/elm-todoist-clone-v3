module UI.Modal exposing (container, form, overlay)

import Css exposing (..)
import Html.Styled as H exposing (Attribute, Html, div)
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


form : List (Attribute msg) -> List (Html msg) -> Html msg
form attrs =
    H.form
        (class "modal-content box"
            :: css [ displayFlex, flexDirection column, padding zero ]
            :: attrs
        )
