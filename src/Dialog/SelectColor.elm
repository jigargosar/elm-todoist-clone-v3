module Dialog.SelectColor exposing (Model, initial, view)

import Html.Styled exposing (div, text)
import Html.Styled.Attributes exposing (css, tabindex)
import Px
import Styles exposing (..)
import Theme


type CColor
    = Blue
    | Green
    | Yellow


initial : Model
initial =
    Model Blue False


type alias Model =
    { color : CColor, open : Bool }


view : Model -> Html.Styled.Html msg
view model =
    div
        [ css [ Px.pa 4, lh 1.5, boAll, boColor Theme.borderGray ]
        , tabindex 0
        ]
        [ viewItem model.color ]


viewItem : CColor -> Html.Styled.Html msg
viewItem color =
    div [] [ text <| colorText color ]


colorText color =
    case color of
        Blue ->
            "Blue"

        Green ->
            "Green"

        Yellow ->
            "Yellow"
