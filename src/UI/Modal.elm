module UI.Modal exposing (..)

import Css exposing (..)
import Html.Styled exposing (Attribute, Html, div)
import Html.Styled.Attributes exposing (class, css)


view : List (Html msg) -> Html msg
view content =
    container (overlay [] :: content)


container : List (Html msg) -> Html msg
container =
    div [ class "modal is-active" ]


overlay : List (Attribute msg) -> Html msg
overlay attrs =
    styled2 div (backgroundColor (hsla 0 0 0 0.2) :: []) "modal-background" attrs []


styled2 : (List (Attribute msg) -> a) -> List Style -> String -> List (Attribute msg) -> a
styled2 func styles className attrs =
    func (css styles :: class className :: attrs)
