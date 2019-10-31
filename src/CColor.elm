module CColor exposing (..)

import Css exposing (hex)


type CColor
    = Blue
    | Green
    | Yellow


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
