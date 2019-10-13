module Drawer exposing (Drawer, Msg, initial, update, view)

import Css
import DnDList
import ExpansionPanel exposing (ExpansionPanel)
import Html.Styled as H exposing (..)
import Html.Styled.Attributes exposing (class, css)
import Lens
import Project exposing (Project)
import Styles exposing (..)


config : DnDList.Config Project
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Vertical
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


system =
    DnDList.create config DndProject


type Drawer
    = Drawer Internal


type alias Internal =
    { projects : ExpansionPanel
    , labels : ExpansionPanel
    , filters : ExpansionPanel
    , dnd : DnDList.Model
    }


initial : Drawer
initial =
    Internal projectsEPS.initial
        labelsEPS.initial
        filtersEPS.initial
        system.model
        |> Drawer


type Panel
    = Projects
    | Labels
    | Filters


type Msg
    = ExpansionPanel Panel ExpansionPanel.Msg
    | DndProject DnDList.Msg


lens : Lens.Config small Internal -> Lens.System small Drawer
lens =
    let
        unwrap : Drawer -> Internal
        unwrap (Drawer internal) =
            internal
    in
    Lens.compose (Lens.system { get = unwrap, set = \s _ -> Drawer s }) << Lens.system


dndL =
    lens { get = .dnd, set = \s b -> { b | dnd = s } }


projectsEPS : ExpansionPanel.System Msg Drawer
projectsEPS =
    let
        projectsLens : Lens.System ExpansionPanel Drawer
        projectsLens =
            lens { get = .projects, set = \s b -> { b | projects = s } }
    in
    ExpansionPanel.system (ExpansionPanel Projects) projectsLens


labelsEPS : ExpansionPanel.System Msg Drawer
labelsEPS =
    let
        labelsLens : Lens.System ExpansionPanel Drawer
        labelsLens =
            lens { get = .labels, set = \s b -> { b | labels = s } }
    in
    ExpansionPanel.system (ExpansionPanel Labels) labelsLens


filtersEPS : ExpansionPanel.System Msg Drawer
filtersEPS =
    let
        filtersLens : Lens.System ExpansionPanel Drawer
        filtersLens =
            lens { get = .filters, set = \s b -> { b | filters = s } }
    in
    ExpansionPanel.system (ExpansionPanel Filters) filtersLens


updatePanel : Panel -> ExpansionPanel.Msg -> Drawer -> ( Drawer, Cmd Msg )
updatePanel panel =
    case panel of
        Projects ->
            projectsEPS.update

        Labels ->
            labelsEPS.update

        Filters ->
            filtersEPS.update


update : (Msg -> msg) -> List Project -> Msg -> Drawer -> ( Drawer, Cmd msg )
update toMsg projectList message model =
    case message of
        ExpansionPanel panel msg ->
            updatePanel panel msg model
                |> Tuple.mapSecond (Cmd.map toMsg)

        DndProject msg ->
            let
                oldDnd =
                    dndL.get model

                ( dnd, items ) =
                    system.update msg oldDnd projectList

                _ =
                    Debug.log "dndInfo" (system.info oldDnd)
            in
            ( dndL.set dnd model
            , Cmd.batch
                [ system.commands oldDnd |> Cmd.map toMsg
                ]
            )


view : (Msg -> msg) -> List Project -> Drawer -> List (Html msg)
view toMsg projectList model =
    [ navIconItem "Inbox" "inbox"
    , navIconItem "Today" "calendar_today"
    , navIconItem "Next 7 Days" "view_week"
    , projectsEPS.view
        "Projects"
        (List.map (navProjectItem (dndL.get model)) projectList)
        model
    , labelsEPS.view
        "Labels"
        [ navLabelItem "to read" 333
        , navLabelItem "medical" 93990
        , navLabelItem "quick-ref" 444
        ]
        model
    , filtersEPS.view
        "Filters"
        [ navFilterItem "Assigned to me" 933
        , navFilterItem "Assigned to others" 9354
        , navFilterItem "Priority 1" 93344
        , navFilterItem "Priority 2" 932323
        , navFilterItem "Priority 3" 932323
        , navFilterItem "View all" 932325
        , navFilterItem "No due date" 9355
        ]
        model
    ]
        |> List.map (H.map toMsg)


navItem title iconColor iconName =
    div [ css [ ph 1, pointer, flex, c_grayL 0.3 ] ]
        [ i
            [ css [ pv 2, ph 1, flex, itemsCenter, c_ iconColor ]
            , class "material-icons"
            ]
            [ text iconName ]
        , div
            [ css [ pv 2, ph 1, flex, itemsCenter, mr 3 ]
            ]
            [ text title ]
        ]


navIconItem title icon =
    navItem title Css.inherit icon


navProjectItem dnd project =
    let
        title =
            Project.title project

        hue =
            Project.hue project |> toFloat
    in
    navItem title (Css.hsl hue 0.7 0.5) "folder"


navLabelItem title hue =
    navItem title (Css.hsl hue 0.7 0.5) "label"


navFilterItem title hue =
    navItem title (Css.hsl hue 0.7 0.5) "filter_list"
