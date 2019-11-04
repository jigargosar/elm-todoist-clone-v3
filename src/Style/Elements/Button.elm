module Style.Elements.Button exposing (basic, link, primary)

import Css exposing (..)
import Css.More as CM exposing (..)
import Css.Transitions as T
import Theme


basic : Style
basic =
    batch
        [ appearanceNone
        , backgroundColorWhite
        , border2 (rem 0.05) solid
        , CM.borderColor Theme.primary
        , borderRadius (rem 0.1)
        , CM.color Theme.primary
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
        , hover
            [ CM.backgroundColor (Theme.primaryWhiten 35)
            , CM.borderColor Theme.primary
            ]
        , focus
            [ boxShadow5 zero
                zero
                zero
                (rem 0.2)
                (fromColorWithAlpha Theme.primary 0.5)
            ]
        , active
            [ textDecorationNone
            , colorWhite
            , CM.backgroundColor (Theme.primaryBlacken 5)
            , CM.borderColor (Theme.primaryBlacken 5)
            ]
        ]


link : Style
link =
    batch
        [ basic
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


primary : Style
primary =
    batch
        [ basic
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
