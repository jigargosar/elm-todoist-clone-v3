module CColor exposing (CColor(..), decoder, infoOld, list, toColor)

import Color exposing (Color)
import Css exposing (hex)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Palette.X11


type CColor
    = Blue
    | Green
    | Yellow


default =
    Blue


toHexString : CColor -> String
toHexString color =
    case color of
        Blue ->
            "#4073ff"

        Green ->
            "#299438"

        Yellow ->
            "#fad000"


fromInt idx =
    case idx of
        1 ->
            Blue

        2 ->
            Green

        3 ->
            Yellow

        _ ->
            Blue


toInt : CColor -> Int
toInt cColor =
    case cColor of
        Blue ->
            1

        Green ->
            2

        Yellow ->
            3


list : List CColor
list =
    [ Blue, Green, Yellow ]


infoOld : CColor -> ( Css.Color, String )
infoOld model =
    let
        cssColor : Css.Color
        cssColor =
            hex (toHexString model)
    in
    case model of
        Blue ->
            ( cssColor, "Blue" )

        Green ->
            ( cssColor, "Green" )

        Yellow ->
            ( cssColor, "Yellow" )


decoder : Decoder CColor
decoder =
    JD.int |> JD.map fromInt


encoder : CColor -> Value
encoder =
    toInt >> JE.int


toColor : CColor -> Color
toColor cColor =
    toHexString cColor
        |> Color.fromHex
        |> Result.withDefault Palette.X11.black
