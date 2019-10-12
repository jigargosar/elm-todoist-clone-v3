module ExpansionPanel exposing
    ( ExpansionPanel
    , Msg
    , System
    , initial
    , system
    , update
    , view
    , viewHeader
    )

import Css
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import MaterialIcons as MI
import Styles exposing (..)


type alias System msg =
    { initial : ExpansionPanel
    , update : Msg -> ExpansionPanel -> ( ExpansionPanel, Cmd msg )
    , viewHeader : String -> ExpansionPanel -> Html msg
    , viewContainer : Html msg -> List (Html msg) -> ExpansionPanel -> Html msg
    , view : String -> List (Html msg) -> ExpansionPanel -> Html msg
    }


system : (Msg -> msg) -> System msg
system toMsg =
    { initial = initial
    , update = update toMsg
    , viewHeader = viewHeader toMsg
    , viewContainer = view
    , view = \title content model -> view (viewHeader toMsg title model) content model
    }


type ExpansionPanel
    = ExpansionPanel Internal


type alias Internal =
    { collapsed : Bool }


initial =
    Internal False |> ExpansionPanel


collapsed : ExpansionPanel -> Bool
collapsed =
    unwrap >> .collapsed


type Msg
    = Toggle


unwrap : ExpansionPanel -> Internal
unwrap (ExpansionPanel internal) =
    internal


map : (Internal -> Internal) -> ExpansionPanel -> ExpansionPanel
map func =
    unwrap >> func >> ExpansionPanel


update : (Msg -> msg) -> Msg -> ExpansionPanel -> ( ExpansionPanel, Cmd msg )
update toMsg message model =
    case message of
        Toggle ->
            ( map (\i -> { i | collapsed = not i.collapsed }) model, Cmd.none )


iBtnStyle =
    batch [ btnReset, pointer ]


viewHeader : (Msg -> msg) -> String -> ExpansionPanel -> Html msg
viewHeader toMsg title model =
    let
        isCollapsed =
            collapsed model
    in
    div
        [ css [ bo_b, boc (grayL 0.9), flex, hover [ bgGrayL 0.95 ] ] ]
        [ button
            [ css [ iBtnStyle, pa 1, flexGrow1 ], onClick (toMsg Toggle) ]
            [ span
                [ css
                    [ c_grayL 0.6
                    , batch
                        [ styleIf isCollapsed [ Css.transforms [ Css.rotate (Css.deg -90) ] ]
                        , transition [ Transitions.transform 200 ]
                        ]
                    ]
                ]
                [ MI.expand_more ]
            , styled span [ bold, pa 1 ] [] [ text title ]
            ]
        , button [ css [ iBtnStyle ] ] [ MI.add ]
        ]


view : Html msg -> List (Html msg) -> ExpansionPanel -> Html msg
view headerHtml contentHtml model =
    let
        isCollapsed =
            collapsed model

        finalContent =
            if isCollapsed then
                []

            else
                contentHtml
    in
    div [] (headerHtml :: finalContent)
