module UI.Modal exposing (..)

import Css exposing (..)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (class, css)


view : List (Html msg) -> Html msg
view content =
    div [ class "modal is-active" ]
        [ div [ css [ backgroundColor (hsla 0 0 0 0.2) ], class "modal-background" ] []
        , div
            [ css
                [ width (px 300), maxWidth (pct 100)

                {- , height (pct 200) -}
                ]
            , class "modal-content"
            ]
            content
        ]
