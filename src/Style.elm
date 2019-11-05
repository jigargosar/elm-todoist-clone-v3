module Style exposing (formGroup, formInput, formLabel, linkBtn, primaryBtn)

import Css exposing (..)
import Css.More exposing (..)
import Css.Transitions as T
import Theme


defaultBorder2 =
    border2 (sRem 0.05) solid


defaultTransitions =
    transitionWithDelay 200 [ T.background, T.border, T.boxShadow, T.color ]


rem =
    ()


focusBoxShadow =
    focus
        [ boxShadow5 zero
            zero
            zero
            (sRem 0.1)
            (Theme.primaryAlpha 0.2)
        ]


basicBtn : Style
basicBtn =
    batch
        [ appearanceNone
        , backgroundColorWhite
        , defaultBorder2
        , borderColorWhite
        , borderRadius (sRem 0.1)
        , color Theme.primary
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
        , focusBoxShadow
        ]


linkBtn : Style
linkBtn =
    batch
        [ basicBtn
        , backgroundColorWhite
        , borderColorTransparent
        , color Theme.primary
        , hover [ color Theme.primaryBlacker ]
        , focus [ color Theme.primaryBlacker ]
        , active [ color Theme.primaryBlacker ]
        ]


primaryBtn : Style
primaryBtn =
    batch
        [ basicBtn
        , backgroundColor Theme.primary
        , borderColor Theme.primary
        , colorWhite
        , hover [ backgroundColor Theme.primaryBlacker ]
        , focus [ backgroundColor Theme.primaryBlacker ]
        , active [ backgroundColor Theme.primaryBlackest ]
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
        , focusBoxShadow
        , focus [ borderColor Theme.primary ]
        ]
