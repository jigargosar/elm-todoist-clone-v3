module Dialog.SelectColor exposing (Model, initial, view)

import Css exposing (hex)
import Html.Styled exposing (Html, div, i, text)
import Html.Styled.Attributes exposing (class, css, tabindex)
import Px
import Styles exposing (..)
import Theme


type CColor
    = Blue
    | Green
    | Yellow


initial : Model
initial =
    Model Blue True


type alias Model =
    { color : CColor, open : Bool }


view : Model -> Html msg
view model =
    div
        [ css [ Px.pa 4, lh 1.5, boAll, boColor Theme.borderGray ]
        , tabindex 0
        ]
        [ viewItem model.color ]


viewItem : CColor -> Html msg
viewItem color =
    div [ css [ flex, relative ] ]
        [ i [ css [ Px.p2 0 4, c_ <| colorCssValue color ], class "material-icons" ] [ text "folder" ]
        , div [ css [ Px.p2 0 4 ] ] [ text <| colorText color ]
        ]


colorText : CColor -> String
colorText =
    colorInfo >> Tuple.second


colorCssValue : CColor -> Css.Color
colorCssValue =
    colorInfo >> Tuple.first


colorInfo : CColor -> ( Css.Color, String )
colorInfo color =
    case color of
        Blue ->
            ( hex "#4073ff", "Blue" )

        Green ->
            ( hex "#299438", "Green" )

        Yellow ->
            ( hex "#fad000", "Yellow" )
