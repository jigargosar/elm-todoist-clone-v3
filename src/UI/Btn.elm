module UI.Btn exposing (..)

import Css exposing (..)
import Css.Transitions as T
import Theme


transitionWithDelay delay =
    List.map (\t -> t delay) >> T.transition


btnS =
    [ backgroundColor Theme.white
    , border3 (rem 0.5) solid Theme.primary
    , borderRadius (rem 0.1)
    , color Theme.primary
    , cursor pointer
    , display inlineBlock
    , fontSize (rem 0.8)
    , height (rem 1.8)
    , lineHeight (rem 1.2)
    , outline zero
    , padding2 (rem 0.25) (rem 0.4)
    , textAlign center
    , textDecoration none
    , transitionWithDelay 200 [ T.background, T.border, T.boxShadow, T.color ]
    , property "user-select" "none"
    , verticalAlign middle
    , whiteSpace noWrap
    ]
