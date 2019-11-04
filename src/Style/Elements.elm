module Style.Elements exposing (..)

import Css exposing (Style)
import Style.Elements.Button as Button


btn : Style
btn =
    Button.basic


primaryBtn : Style
primaryBtn =
    Button.primary


linkBtn : Style
linkBtn =
    Button.link
