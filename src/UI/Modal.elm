module UI.Modal exposing (..)

import Css exposing (..)
import Html.Styled exposing (Attribute, Html, div)
import Html.Styled.Attributes exposing (class, css)


view : List (Html msg) -> Html msg
view content =
    div [ class "modal is-active" ]
        (div [ css [ backgroundColor (hsla 0 0 0 0.2) ], class "modal-background" ] []
            :: content
        )


container : List (Html msg) -> Html msg
container =
    div [ class "modal is-active" ]


overlay : List Style -> List (Attribute msg) -> List (Html msg) -> Html msg
overlay styles =
    styled2 div (backgroundColor (hsla 0 0 0 0.2) :: styles) "modal-background"


styled2 : (List (Attribute msg) -> a) -> List Style -> String -> List (Attribute msg) -> a
styled2 func styles className attrs =
    func (css styles :: class className :: attrs)
