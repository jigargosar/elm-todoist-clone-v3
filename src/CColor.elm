module CColor exposing (CColor(..), decoder, infoOld, list, toColor, toCssColor, toName)

import Color exposing (Color)
import Css
import Json.Decode as JD exposing (Decoder)


type CColor
    = Blue
    | Green
    | Yellow
    | Charcoal
    | Red


list : List CColor
list =
    [ Blue, Green, Yellow, Charcoal, Red ]


default : CColor
default =
    Blue


toColor : CColor -> Color
toColor model =
    case model of
        Blue ->
            rgb ( 64, 115, 255 )

        Green ->
            rgb ( 41, 148, 56 )

        Yellow ->
            rgb ( 250, 208, 0 )

        Charcoal ->
            rgb ( 128, 128, 128 )

        Red ->
            rgb ( 219, 64, 53 )


fromInt : Int -> CColor
fromInt idx =
    case idx of
        1 ->
            Blue

        2 ->
            Green

        3 ->
            Yellow

        4 ->
            Red

        _ ->
            default


toName : CColor -> String
toName model =
    case model of
        Blue ->
            "Blue"

        Green ->
            "Green"

        Yellow ->
            "Yellow"

        Charcoal ->
            "Charcoal"

        Red ->
            "Red"


infoOld : CColor -> ( Css.Color, String )
infoOld model =
    ( toCssColor model, toName model )


decoder : Decoder CColor
decoder =
    JD.int |> JD.map fromInt


toCssColor : CColor -> Css.Color
toCssColor model =
    let
        ( r, g, b ) =
            Color.toRGB (toColor model)
    in
    Css.rgb (round r) (round g) (round b)


rgb : ( Float, Float, Float ) -> Color
rgb =
    Color.fromRGB


type alias RGB =
    ( Int, Int, Int )
