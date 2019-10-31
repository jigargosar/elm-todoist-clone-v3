module CColor exposing (..)

import Css exposing (hex)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


type CColor
    = Blue
    | Green
    | Yellow


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


info : CColor -> ( Css.Color, String )
info color =
    case color of
        Blue ->
            ( hex "#4073ff", "Blue" )

        Green ->
            ( hex "#299438", "Green" )

        Yellow ->
            ( hex "#fad000", "Yellow" )


decoder : Decoder CColor
decoder =
    JD.int |> JD.map fromInt


encoder : CColor -> Value
encoder =
    toInt >> JE.int
