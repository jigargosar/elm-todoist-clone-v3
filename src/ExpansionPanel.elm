module ExpansionPanel exposing (Config, ExpansionPanel, Msg, createConfig, subscriptions, update, view)

import Html.Styled exposing (Attribute, Html, button, div, i, span, text)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events exposing (onClick)
import Px
import Styles exposing (..)
import Theme


type ExpansionPanel
    = ExpansionPanel State


type alias State =
    Bool


unwrap : ExpansionPanel -> State
unwrap (ExpansionPanel state) =
    state


map : (State -> State) -> ExpansionPanel -> ExpansionPanel
map func =
    unwrap >> func >> ExpansionPanel


type Msg
    = Toggled


type alias Config msg =
    { toggled : msg, title : String, secondary : { iconName : String, action : msg } }


type alias ToMsg msg =
    Msg -> msg


createConfig :
    ToMsg msg
    ->
        { title : String
        , secondary : { iconName : String, action : msg }
        }
    -> Config msg
createConfig toMsg { title, secondary } =
    { toggled = toMsg Toggled
    , title = title
    , secondary = secondary
    }


subscriptions : Config msg -> ExpansionPanel -> Sub msg
subscriptions _ _ =
    Sub.none


update : Config msg -> Msg -> ExpansionPanel -> ( ExpansionPanel, Cmd msg )
update _ message model =
    case message of
        Toggled ->
            ( map not model, Cmd.none )


view : Config msg -> (() -> List (Html msg)) -> ExpansionPanel -> List (Html msg)
view config content (ExpansionPanel collapsed) =
    viewExpansionPanelHeader config collapsed
        :: (if collapsed then
                []

            else
                content ()
           )


viewExpansionPanelHeader : Config msg -> Bool -> Html msg
viewExpansionPanelHeader { toggled, title, secondary } collapsed =
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
                if collapsed then
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
