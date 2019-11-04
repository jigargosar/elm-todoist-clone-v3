module UI.Btn exposing (linkStyle, primaryStyle, style)

import Css exposing (..)
import Css.More exposing (fromColor, fromColorWithAlpha)
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
        , border3 (rem 0.05) solid (fromColor Theme.primary)
        , borderRadius (rem 0.1)
        , color (fromColor Theme.primary)
        , cursor pointer
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
    , backgroundColor (fromColor (Theme.primaryBlacken 5))
    , borderColor (fromColor (Theme.primaryBlacken 5))
    ]


hoverStyles =
    [ backgroundColor (fromColor (Theme.primaryWhiten 35))
    , borderColor (fromColor Theme.primary)
    ]


focusStyles =
    [ boxShadow5 zero
        zero
        zero
        (rem 0.2)
        (fromColorWithAlpha Theme.primary 0.5)
    ]


linkStyle : Style
linkStyle =
    batch
        [ backgroundColor Theme.white
        , borderColor transparent
        , color (fromColor Theme.primary)
        , hover
            [ color (fromColor (Theme.primaryBlacken 10))
            , backgroundColor Theme.white
            , borderColor transparent
            ]
        , active
            [ color (fromColor (Theme.primaryBlacken 10))
            , backgroundColor Theme.white
            , borderColor transparent
            ]
        ]


primaryStyle : Style
primaryStyle =
    batch
        [ backgroundColor (fromColor Theme.primary)
        , borderColor transparent
        , color Theme.white
        , hover
            [ color Theme.white
            , backgroundColor (fromColor (Theme.primaryBlacken 5))
            , borderColor (fromColor (Theme.primaryBlacken 5))
            ]
        , active
            [ color Theme.white
            , backgroundColor (fromColor (Theme.primaryBlacken 10))
            , borderColor (fromColor (Theme.primaryBlacken 10))
            ]
        ]
