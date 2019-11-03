module UI.IconButton exposing (..)

import CColor
import Css.Transitions as CT
import Html.Styled exposing (Html, button)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Px
import Styles exposing (..)
import Theme
import UI.Icon as Icon exposing (Icon)


view : Icon -> msg -> Html msg
view icon action =
    button
        [ css
            [ btnReset
            , pointer
            , Px.pa 4
            , boRad 2
            , fg <| CColor.toCssColor CColor.Charcoal
            , hover
                [ bg Theme.hoverGray
                , fgInherit
                ]
            , CT.transition
                [ CT.background 150
                ]
            ]
        , onClick action
        ]
        [ Icon.view icon ]
