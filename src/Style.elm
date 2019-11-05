module Style exposing (formGroup, formInput, formLabel, linkBtn, primaryBtn)

import Css exposing (..)
import Css.More as CM exposing (..)
import Css.Transitions as T
import Theme


defaultBorder2 =
    border2 (sRem 0.05) solid


defaultTransitions =
    transitionWithDelay 200 [ T.background, T.border, T.boxShadow, T.color ]


rem =
    ()


basicBtn : Style
basicBtn =
    batch
        [ appearanceNone
        , backgroundColorWhite
        , defaultBorder2
        , borderColorWhite
        , borderRadius (sRem 0.1)
        , CM.color Theme.primary
        , cursor pointer
        , display inlineBlock
        , fontSize (sRem 0.8)
        , height (sRem 1.8)
        , lineHeight (sRem 1.2)
        , outline zero
        , padding2 (sRem 0.25) (sRem 0.4)
        , textAlign center
        , textDecorationNone
        , defaultTransitions
        , userSelectNone
        , verticalAlign middle
        , whiteSpace noWrap
        , focus
            [ boxShadow5 zero
                zero
                zero
                (sRem 0.1)
                (fromColorWithAlpha Theme.primary 0.2)
            ]
        ]


linkBtn : Style
linkBtn =
    batch
        [ basicBtn
        , backgroundColorWhite
        , borderColorTransparent
        , CM.color Theme.primary
        , hover
            [ CM.color (Theme.primaryBlacken 10)
            ]
        , focus
            [ CM.color (Theme.primaryBlacken 10)
            ]
        , active
            [ CM.color (Theme.primaryBlacken 10)
            ]
        ]


primaryBtn : Style
primaryBtn =
    batch
        [ basicBtn
        , CM.backgroundColor Theme.primary
        , CM.borderColor Theme.primary
        , colorWhite
        , hover
            [ CM.backgroundColor (Theme.primaryBlacken 5)
            ]
        , focus
            [ CM.backgroundColor (Theme.primaryBlacken 5)
            ]
        , active
            [ CM.backgroundColor (Theme.primaryBlacken 10)
            ]
        ]


formGroup : Style
formGroup =
    batch
        [ lastChild [ marginBottom zero ]
        , marginBottom (sRem 0.4)
        ]


formLabel : Style
formLabel =
    batch
        [ display block
        , lineHeight (sRem 1.2)
        , padding2 (sRem 0.3) zero
        ]


formInput : Style
formInput =
    batch
        [ appearanceNone
        , backgroundColorWhite
        , backgroundImage none
        , defaultBorder2
        , borderColorHSL 217 0.16 0.77
        , colorHSL 218 0.16 0.27
        , display block
        , fontSize (sRem 0.8)
        , height (sRem 1.8)
        , lineHeight (sRem 1.2)
        , maxWidth (pct 100)
        , outline none
        , padding2 (sRem 0.25) (sRem 0.4)
        , position relative
        , defaultTransitions
        , width (pct 100)
        ]
