module CColor exposing (CColor(..), decoder, infoOld, list, toColor, toCssColor, toName)

import Color exposing (Color)
import Css exposing (hex)
import Json.Decode as JD exposing (Decoder)
import Palette.X11


type CColor
    = Blue
    | Green
    | Yellow
    | Charcoal


default : CColor
default =
    Blue


toColor : CColor -> Color
toColor model =
    case model of
        Blue ->
            rgb ( 128, 128, 128 )

        Green ->
            rgb ( 128, 128, 128 )

        Yellow ->
            rgb ( 128, 128, 128 )

        Charcoal ->
            rgb ( 128, 128, 128 )


rgb =
    Color.fromRGB


type alias RGB =
    ( Int, Int, Int )


fromInt : Int -> CColor
fromInt idx =
    case idx of
        1 ->
            Blue

        2 ->
            Green

        3 ->
            Yellow

        _ ->
            default


list : List CColor
list =
    [ Blue, Green, Yellow ]


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
