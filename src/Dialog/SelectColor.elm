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
        [ text "select colors" ]
