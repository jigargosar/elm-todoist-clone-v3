module Drawer exposing (Drawer, Msg, initial, update, view)

import Css
import ExpansionPanel exposing (ExpansionPanel)
import Html.Styled as H exposing (..)
import Html.Styled.Attributes exposing (css)
import Lens
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
    Internal projectsEPS.initial
        labelsEPS.initial
        filtersEPS.initial
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


projectsEPS : ExpansionPanel.System Msg Drawer
projectsEPS =
    let
        projectsLens : Lens.System ExpansionPanel Drawer
        projectsLens =
            internalLens { get = .projects, set = \s b -> { b | projects = s } }
    in
    ExpansionPanel.system (ExpansionPanel Projects) projectsLens


labelsEPS : ExpansionPanel.System Msg Drawer
labelsEPS =
    let
        labelsLens : Lens.System ExpansionPanel Drawer
        labelsLens =
            internalLens { get = .labels, set = \s b -> { b | labels = s } }
    in
    ExpansionPanel.system (ExpansionPanel Labels) labelsLens


filtersEPS : ExpansionPanel.System Msg Drawer
filtersEPS =
    let
        filtersLens : Lens.System ExpansionPanel Drawer
        filtersLens =
            internalLens { get = .filters, set = \s b -> { b | filters = s } }
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


update : (Msg -> msg) -> Msg -> Drawer -> ( Drawer, Cmd msg )
update toMsg message model =
    case message of
        ExpansionPanel panel msg ->
            updatePanel panel msg model
                |> Tuple.mapSecond (Cmd.map toMsg)


view : (Msg -> msg) -> Drawer -> List (Html msg)
view toMsg model =
    [ navIconItem "Inbox" MI.inbox
    , navIconItem "Today" MI.calendar_today
    , navIconItem "Next 7 Days" MI.view_week
    , projectsEPS.view
        "Projects"
        [ navProjectItem "Clone This" (Css.hsl 10 0.7 0.5)
        , navProjectItem "Learn This" (Css.hsl 50 0.7 0.5)
        , navProjectItem "Finish That" (Css.hsl 170 0.7 0.5)
        ]
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


subItem title =
    styled div
        [ pa 2, pointer ]
        []
        [ text title
        ]


navItem title =
    styled div [ pa 2, pointer ] [] [ text title ]


navIconItem title icon =
    div [ css [ ph 1, pointer, flex, c_grayL 0.3 ] ]
        [ div [ css [ pv 2, ph 1, flex, itemsCenter ] ] [ icon ]
        , div [ css [ pv 2, ph 1, flex, itemsCenter ] ] [ text title ]
        ]


navProjectItem title color =
    div [ css [ ph 1, pointer, flex, c_grayL 0.3 ] ]
        [ div [ css [ pv 2, ph 1, flex, itemsCenter, c_ color ] ] [ MI.folder ]
        , div [ css [ pv 2, ph 1, flex, itemsCenter ] ] [ text title ]
        ]


navLabelItem title hue =
    div [ css [ ph 1, pointer, flex, c_grayL 0.3 ] ]
        [ div [ css [ pv 2, ph 1, flex, itemsCenter, c_ (Css.hsl hue 0.7 0.5) ] ] [ MI.label ]
        , div [ css [ pv 2, ph 1, flex, itemsCenter ] ] [ text title ]
        ]


navFilterItem title hue =
    div [ css [ ph 1, pointer, flex, c_grayL 0.3 ] ]
        [ div [ css [ pv 2, ph 1, flex, itemsCenter, c_ (Css.hsl hue 0.7 0.5) ] ] [ MI.filter_list ]
        , div [ css [ pv 2, ph 1, flex, itemsCenter ] ] [ text title ]
        ]
