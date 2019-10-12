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
    let
        unwrap : Drawer -> Internal
        unwrap (Drawer internal) =
            internal
    in
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


updatePanel : Panel -> ExpansionPanel.Msg -> Drawer -> ( Drawer, Cmd Msg )
updatePanel panel =
    case panel of
        Projects ->
            projectsEPS.update

        Labels ->
            labelsEPS.update

        Filters ->
            filtersEPS.update


update : (Msg -> msg) -> Msg -> Drawer -> ( Drawer, Cmd msg )
update toMsg message model =
    case message of
        ExpansionPanel panel msg ->
            updatePanel panel msg model
                |> Tuple.mapSecond (Cmd.map toMsg)


view : (Msg -> msg) -> Drawer -> List (Html msg)
view toMsg model =
    [ navItem "Inbox"
    , navItem "Today"
    , navItem "Next 7 Days"
    , projectsEPS.view
        "Projects"
        [ subItem "FooBar"
        , subItem "Learn This"
        ]
        model
    , labelsEPS.view
        "Labels"
        [ subItem "to read"
        , subItem "medical"
        , subItem "quick-ref"
        ]
        model
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
