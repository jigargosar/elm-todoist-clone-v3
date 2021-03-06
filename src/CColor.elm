module CColor exposing (CColor(..), decoder, default, infoOld, list, name, toColor, toCssColor)

import Color exposing (Color)
import Compare exposing (Comparator)
import Css
import Json.Decode as JD exposing (Decoder)
import Tuple3


type CColor
    = Blue
    | Green
    | Yellow
    | Charcoal
    | Red
    | Orange


appendOnlyList : List CColor
appendOnlyList =
    [ Blue
    , Green
    , Yellow
    , Charcoal
    , Red
    , Orange
    ]


fromInt : Int -> CColor
fromInt idx =
    List.drop idx appendOnlyList |> List.head |> Maybe.withDefault default


default : CColor
default =
    Blue


comparator : Comparator CColor
comparator =
    let
        hslT3Comparator : Comparator ( Float, Float, Float )
        hslT3Comparator =
            [ Tuple3.first >> (*) -1, Tuple3.second, Tuple3.third ]
                |> List.map Compare.by
                |> Compare.concat
    in
    Compare.compose (toColor >> Color.toHSL) hslT3Comparator


list : List CColor
list =
    List.sortWith comparator appendOnlyList


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

        Orange ->
            rgb ( 255, 153, 51 )


name : CColor -> String
name model =
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

        Orange ->
            "Orange"


infoOld : CColor -> ( Css.Color, String )
infoOld model =
    ( toCssColor model, name model )


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
