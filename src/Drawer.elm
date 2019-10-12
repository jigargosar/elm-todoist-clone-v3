module Drawer exposing (Drawer, Msg, initial, update, view)

import ExpansionPanel exposing (ExpansionPanel)
import Html.Styled as H exposing (..)
import Lens
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


internalLens : Lens.Config small Internal -> Lens.System small Drawer
internalLens =
    Lens.compose (Lens.system { get = unwrap, set = \s _ -> Drawer s }) << Lens.system


projectsEPS : ExpansionPanel.SystemL Msg Drawer
projectsEPS =
    let
        projectsLens : Lens.System ExpansionPanel Drawer
        projectsLens =
            internalLens { get = .projects, set = \s b -> { b | projects = s } }
    in
    ExpansionPanel.systemL (ExpansionPanel Projects) projectsLens


labelsEPS : ExpansionPanel.SystemL Msg Drawer
labelsEPS =
    let
        labelsLens : Lens.System ExpansionPanel Drawer
        labelsLens =
            internalLens { get = .labels, set = \s b -> { b | labels = s } }
    in
    ExpansionPanel.systemL (ExpansionPanel Labels) labelsLens


filtersEPS : ExpansionPanel.SystemL Msg Drawer
filtersEPS =
    let
        filtersLens : Lens.System ExpansionPanel Drawer
        filtersLens =
            internalLens { get = .filters, set = \s b -> { b | filters = s } }
    in
    ExpansionPanel.systemL (ExpansionPanel Filters) filtersLens


unwrap : Drawer -> Internal
unwrap (Drawer internal) =
    internal


updateInternal : (Internal -> ( Internal, Cmd msg )) -> Drawer -> ( Drawer, Cmd msg )
updateInternal func =
    unwrap >> func >> Tuple.mapFirst Drawer


updatePanel : Panel -> ExpansionPanel.Msg -> Drawer -> ( Drawer, Cmd Msg )
updatePanel panel message model =
    case panel of
        Projects ->
            projectsEPS.update message model

        --        Projects ->
        --            updateInternal
        --                (\i ->
        --                    projectsEPS.update message i.projects
        --                        |> Tuple.mapFirst (\s -> { i | projects = s })
        --                )
        --                model
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
