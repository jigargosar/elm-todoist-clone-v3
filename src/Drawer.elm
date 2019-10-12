module Drawer exposing (Drawer, Msg, initial, update, view)

import Css
import Css.Transitions as Transitions exposing (transition)
import ExpansionPanel exposing (ExpansionPanel)
import Html.Styled as H exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import MaterialIcons as MI
import Styles exposing (..)


type Drawer
    = Drawer Internal


type alias Internal =
    { projects : ExpansionPanel
    , labels : ExpansionPanel
    , filters : ExpansionPanel
    }


initial : Drawer
initial =
    Internal ExpansionPanel.initial
        ExpansionPanel.initial
        ExpansionPanel.initial
        |> Drawer


type Panel
    = Projects
    | Labels
    | Filters


type Msg
    = ExpansionPanel Panel ExpansionPanel.Msg


projectsEPS : ExpansionPanel.System Msg
projectsEPS =
    ExpansionPanel.system (ExpansionPanel Projects)


unwrap : Drawer -> Internal
unwrap (Drawer internal) =
    internal


map : (Internal -> Internal) -> Drawer -> Drawer
map func =
    unwrap >> func >> Drawer


updateInternal : (Internal -> ( Internal, Cmd msg )) -> Drawer -> ( Drawer, Cmd msg )
updateInternal func =
    unwrap >> func >> Tuple.mapFirst Drawer


updatePanel : Panel -> ExpansionPanel.Msg -> Drawer -> ( Drawer, Cmd Msg )
updatePanel panel message model =
    case panel of
        Projects ->
            updateInternal
                (\i ->
                    projectsEPS.update message i.projects
                        |> Tuple.mapFirst (\s -> { i | projects = s })
                )
                model

        Labels ->
            updateInternal
                (\i ->
                    ExpansionPanel.update (ExpansionPanel Labels) message i.labels
                        |> Tuple.mapFirst (\s -> { i | labels = s })
                )
                model

        Filters ->
            updateInternal
                (\i ->
                    ExpansionPanel.update (ExpansionPanel Filters) message i.filters
                        |> Tuple.mapFirst (\s -> { i | filters = s })
                )
                model


update : (Msg -> msg) -> Msg -> Drawer -> ( Drawer, Cmd msg )
update toMsg message model =
    case message of
        ExpansionPanel panel msg ->
            updatePanel panel msg model
                |> Tuple.mapSecond (Cmd.map toMsg)


view : (Msg -> msg) -> Drawer -> List (Html msg)
view toMsg model =
    let
        projectsEP =
            model |> unwrap >> .projects

        labelsEP =
            model |> unwrap >> .labels

        filtersEP =
            model |> unwrap >> .filters
    in
    [ navItem "Inbox"
    , navItem "Today"
    , navItem "Next 7 Days"
    , projectsEPS.view
        "Projects"
        [ subItem "FooBar"
        , subItem "Learn This"
        ]
        projectsEP
    , ExpansionPanel.view
        (ExpansionPanel.viewHeader
            (ExpansionPanel Labels)
            "Labels"
            labelsEP
        )
        [ subItem "to read"
        , subItem "medical"
        , subItem "quick-ref"
        ]
        labelsEP
    ]
        |> List.map (H.map toMsg)


subItem title =
    styled div
        [ pa 2, pointer ]
        []
        [ text title
        ]


navItem title =
    styled div [ pa 2, pointer ] [] [ text title ]


expansionPanel collapsed toggle title content =
    let
        visibleContent =
            if collapsed then
                []

            else
                content
    in
    div []
        (expansionPanelHeader collapsed toggle title
            :: visibleContent
        )


expansionPanelHeader collapsed toggle title =
    div
        [ css [ bo_b, boc (grayL 0.9), flex, hover [ bgGrayL 0.95 ] ] ]
        [ button
            [ css [ iBtnStyle, pa 1, flexGrow1 ], onClick toggle ]
            [ span
                [ css
                    [ c_grayL 0.6
                    , batch
                        [ styleIf collapsed [ Css.transforms [ Css.rotate (Css.deg -90) ] ]
                        , transition [ Transitions.transform 200 ]
                        ]
                    ]
                ]
                [ MI.expand_more ]
            , styled span [ bold, pa 1 ] [] [ text title ]
            ]
        , button [ css [ iBtnStyle ] ] [ MI.add ]
        ]


iBtnStyle =
    batch [ btnReset, pointer ]
