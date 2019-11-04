module Theme exposing (..)

import Color
import Css exposing (Color, rgb)
import Styles


borderGray : Color
borderGray =
    Styles.grayL 0.8


primary : Color.Color
primary =
    Color.fromHSL ( 241, 63, 59 )


primaryWhiten n =
    primary |> Color.whiten n


primaryBlacken n =
    primary |> Color.blacken n
