module UI.Btn exposing (..)

import Color
import Color.Transparent
import Css exposing (..)
import Css.Transitions as T
import Theme


transitionWithDelay delay =
    List.map (\t -> t delay) >> T.transition


textDecorationNone =
    textDecoration none


colorWhite =
    color Theme.white


prefixes =
    [ "-webkit-", "-moz-", "-ms-" ]


prefixedProperty name value =
    prefixes
        |> List.map (\prefix -> property (prefix ++ name) value)
        |> batch


appearanceNone =
    prefixedProperty "appearance" "none"


userSelectNone =
    prefixedProperty "appearance" "none"


style =
    batch
        [ appearanceNone
        , backgroundColor Theme.white
        , border3 (rem 0.5) solid (Theme.colorToCssColor Theme.primary)
        , borderRadius (rem 0.1)
        , color (Theme.colorToCssColor Theme.primary)
        , colorWhite
        , display inlineBlock
        , fontSize (rem 0.8)
        , height (rem 1.8)
        , lineHeight (rem 1.2)
        , outline zero
        , padding2 (rem 0.25) (rem 0.4)
        , textAlign center
        , textDecorationNone
        , transitionWithDelay 200 [ T.background, T.border, T.boxShadow, T.color ]
        , userSelectNone
        , verticalAlign middle
        , whiteSpace noWrap
        , active activeStyles
        , focus focusStyles
        ]


activeStyles =
    [ textDecorationNone
    , colorWhite
    , borderColor (Theme.colorToCssColor (Theme.primary |> Color.whiten 10))
    , backgroundColor (Theme.colorToCssColor (Theme.primary |> Color.whiten 10))
    ]


focusStyles =
    [ boxShadow5 zero
        zero
        zero
        (rem 0.1)
        (Theme.transparentColorToCssColor
            (Color.Transparent.fromColor (Color.Transparent.customOpacity 0.2) Theme.primary)
        )
    ]
