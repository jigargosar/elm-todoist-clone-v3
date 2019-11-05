module Style exposing (basicBtn, formGroup, formLabel, linkBtn, primaryBtn)

import Css exposing (..)
import Css.More as CM exposing (..)
import Css.Transitions as T
import Theme


basicBtn : Style
basicBtn =
    batch
        [ appearanceNone
        , backgroundColorWhite
        , border2 (rem 0.05) solid
        , CM.borderColor Theme.primary
        , borderRadius (rem 0.125)
        , CM.color Theme.primary
        , cursor pointer
        , display inlineBlock
        , fontSize (rem 1)
        , height (rem 2)
        , lineHeight (rem 1.5)
        , outline zero
        , padding2 (rem 0.25) (rem 0.5)
        , textAlign center
        , textDecorationNone
        , transitionWithDelay 200 [ T.background, T.border, T.boxShadow, T.color ]
        , userSelectNone
        , verticalAlign middle
        , whiteSpace noWrap
        , hover
            [ CM.backgroundColor (Theme.primaryWhiten 35)
            , CM.borderColor Theme.primary
            ]
        , focus
            [ boxShadow5 zero
                zero
                zero
                (rem 0.125)
                (fromColorWithAlpha Theme.primary 0.5)
            ]
        , active
            [ textDecorationNone
            , colorWhite
            , CM.backgroundColor (Theme.primaryBlacken 5)
            , CM.borderColor (Theme.primaryBlacken 5)
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
            , backgroundColorWhite
            , borderColorTransparent
            ]
        , active
            [ CM.color (Theme.primaryBlacken 10)
            , backgroundColorWhite
            , borderColorTransparent
            ]
        ]


primaryBtn : Style
primaryBtn =
    batch
        [ basicBtn
        , CM.backgroundColor Theme.primary
        , borderColorTransparent
        , colorWhite
        , hover
            [ colorWhite
            , CM.backgroundColor (Theme.primaryBlacken 5)
            , CM.borderColor (Theme.primaryBlacken 5)
            ]
        , active
            [ colorWhite
            , CM.backgroundColor (Theme.primaryBlacken 10)
            , CM.borderColor (Theme.primaryBlacken 10)
            ]
        ]


formGroup : Style
formGroup =
    batch
        [ lastChild [ marginBottom zero ]
        , marginBottom (rem 0.5)
        ]


formLabel : Style
formLabel =
    batch
        [ display block
        , lineHeight (rem 1.5)
        , padding2 (rem 0.375) zero
        ]


formInput =
    batch [ appearanceNone, backgroundColorWhite, backgroundImage none ]
