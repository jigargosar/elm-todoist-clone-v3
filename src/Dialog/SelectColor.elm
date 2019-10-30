module Dialog.SelectColor exposing (Model, initial, view)

import Html.Styled exposing (div, text)


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
    case model.open of
        True ->
            div [] [ text "open" ]

        False ->
            div [] [ text "closed" ]
