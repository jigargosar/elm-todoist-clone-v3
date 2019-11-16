module UI.Modal exposing (..)

import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (class)


view : List (Html msg) -> Html msg
view content =
    div [ class "modal is-active" ]
        [ div [ class "modal-background" ] []
        , div [ class "modal-content box" ] content
        ]
