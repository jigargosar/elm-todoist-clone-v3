module UI.Btn exposing (..)

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
    prefixedProperty "user-select" "none"


style =
    batch
        [ appearanceNone
        , backgroundColor Theme.white
        , border3 (rem 0.05) solid (Theme.toCssColor Theme.primary)
        , borderRadius (rem 0.1)
        , color (Theme.toCssColor Theme.primary)
        , display inlineBlock
        , fontSize (rem 0.8)
        , fontSize (rem 1)
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
        , hover hoverStyles
        , focus focusStyles
        , active activeStyles
        ]


activeStyles =
    [ textDecorationNone
    , colorWhite
    , backgroundColor (Theme.toCssColor (Theme.primaryBlacken 10))
    , borderColor (Theme.toCssColor (Theme.primaryBlacken 10))
    ]


hoverStyles =
    [ backgroundColor (Theme.toCssColor (Theme.primaryWhiten 35))
    , borderColor (Theme.toCssColor Theme.primary)
    ]


focusStyles =
    [ boxShadow5 zero
        zero
        zero
        (rem 0.2)
        (Theme.toCssColorAlpha (Theme.primaryAlpha 0.2))
    ]
