module Theme exposing (..)

import Css exposing (Color)
import Styles


borderGray : Color
borderGray =
    Styles.grayL 0.8


primary =
    primaryAddBrightness 0


primaryAddBrightness n =
    Css.hsl 241 0.63 (0.59 + n)


primaryBlacker =
    primaryAddBrightness 0.05


primaryBlackest =
    primaryAddBrightness 0.01


primaryAlpha =
    Css.hsla 241 0.63 0.59
