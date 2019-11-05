module ExpansionPanel exposing
    ( Collapsible
    , Config
    , expanded
    , toggle
    , view
    )

import Html.Styled exposing (Attribute, Html, button, div, i, span, text)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events exposing (onClick)
import Px
import Styles exposing (..)
import Theme


type Collapsible
    = Collapsed
    | Expanded


expanded : Collapsible
expanded =
    Expanded


toggle : Collapsible -> Collapsible
toggle model =
    case model of
        Collapsed ->
            Expanded

        Expanded ->
            Collapsed


type alias Config msg =
    { toggled : msg
    , title : String
    , secondary : { iconName : String, action : msg }
    }


view : Config msg -> (() -> List (Html msg)) -> Collapsible -> List (Html msg)
view config content model =
    viewHeader config model
        :: (if model == Expanded then
                content ()

            else
                []
           )


viewHeader : Config msg -> Collapsible -> Html msg
viewHeader { toggled, title, secondary } model =
    div
        [ css
            [ Px.pl 4
            , Px.pr (4 + 16)
            , flex
            , itemsCenter
            , bo_b
            , boColor Theme.borderGray
            , hover [ bgGrayL 0.95 ]
            ]
        ]
        [ let
            iconName =
                if model == Collapsed then
                    "chevron_right"

                else
                    "expand_more"
          in
          button
            [ css [ btnReset, pointer, flex, itemsCenter, flexGrow1, tal ], onClick toggled ]
            [ i [ css [ Px.pa 4 ], class "material-icons" ] [ text iconName ]
            , span [ css [ Px.p2 8 4, bold, flexGrow1 ] ] [ text title ]
            ]
        , button
            [ css [ btnReset, pointer, Px.pa 4, Px.m2 4 0, flex, itemsCenter, selfEnd ]
            , onClick secondary.action
            ]
            [ i [ class "material-icons" ] [ text secondary.iconName ] ]
        ]
