module Style exposing (btnAction, btnLink, drawerItem, formGroup, formInput, formLabel, listItemIcon, listItemLink, primaryBtn)

import Css exposing (..)
import Css.More exposing (..)
import Css.Transitions as T
import Theme


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
        , backgroundColor white
        , defaultBorder2
        , borderColor white
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
        , zIndex (int 1)
        ]


btnLink : Style
btnLink =
    batch
        [ basicBtn
        , backgroundColor white
        , borderColor transparent
        , color Theme.primary
        , hover [ color Theme.primaryBlacker ]
        , focus [ color Theme.primaryBlacker ]
        , active [ color Theme.primaryBlacker ]
        ]


btnAction : Style
btnAction =
    batch [ paddingLeft zero, paddingRight zero, width (sRem 1.8) ]


primaryBtn : Style
primaryBtn =
    batch
        [ basicBtn
        , backgroundColor Theme.primary
        , borderColor Theme.primary
        , color white
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
        , backgroundColor white
        , backgroundImage none
        , defaultBorder2
        , borderColor <| hsl 217 0.16 0.77
        , color <| hsl 218 0.16 0.27
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


drawerItem : Style
drawerItem =
    batch [ padding2 zero (sRem 0.25), paddingRight (sRem (0.25 + 1)), displayFlex ]


listItemIcon : Style
listItemIcon =
    batch [ padding2 (sRem 0.25) (sRem 0.25), verticalAlign middle ]


listItemLink : Style
listItemLink =
    batch
        [ padding2 (sRem 0.25) (sRem 0.25)
        , fontSize (sRem 0.8)

        -- , height (sRem 1.8)
        , lineHeight (sRem 1.2)
        , verticalAlign middle
        ]
