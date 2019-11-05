module Css.More exposing
    ( appearanceNone
    , sRem
    , textDecorationNone
    , transitionWithDelay
    , userSelectNone
    , white
    )

import Css
import Css.Transitions as T


sRem : Float -> Css.Rem
sRem n =
    {-
       base 16
       rem 1
       base 20
       rem 0.8


       base 20
       rem 1.8
       base 16
       rem 2



    -}
    Css.rem ((20 / 16) * n)


white : Css.Color
white =
    Css.rgb 255 255 255


transitionWithDelay : a -> List (a -> T.Transition) -> Css.Style
transitionWithDelay delay =
    List.map (\t -> t delay) >> T.transition


textDecorationNone : Css.Style
textDecorationNone =
    Css.textDecoration Css.none


prefixes =
    [ "-webkit-", "-moz-", "-ms-" ]


prefixedProperty name value =
    prefixes
        |> List.map (\prefix -> Css.property (prefix ++ name) value)
        |> Css.batch


appearanceNone : Css.Style
appearanceNone =
    prefixedProperty "appearance" "none"


userSelectNone : Css.Style
userSelectNone =
    prefixedProperty "user-select" "none"
