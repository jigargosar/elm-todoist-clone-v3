module UI.IconButton exposing (..)

import CColor
import Color
import Css exposing (Color)
import Css.Transitions as CT
import Html.Styled exposing (Html, button)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Px
import Styles exposing (..)
import Theme
import UI.Icon as Icon exposing (Icon)


iconGray : Color
iconGray =
    CColor.toColor CColor.Charcoal
        |> Color.whiten 10
        |> toCssColor


view : Icon -> msg -> Html msg
view icon action =
    button
        [ css
            [ btnReset
            , pointer
            , Px.pa 4
            , boRad 2
            , fg iconGray
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
