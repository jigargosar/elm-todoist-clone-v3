module Drawer exposing (Drawer, initial, update, view)

import Css exposing (transform, transforms)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import MaterialIcons as MI
import Set exposing (Set)
import Styles exposing (..)
import Tagged exposing (Tagged, tag)
import Tagged.Set exposing (TaggedSet)


type Drawer
    = Drawer ExpansionPanelSet


initial : Drawer
initial =
    Drawer Tagged.Set.empty


type ExpansionPanel
    = Projects
    | Labels
    | Filters


type alias ExpansionPanelSet =
    TaggedSet ExpansionPanel String


type alias TaggedExpansionPanel =
    Tagged ExpansionPanel String


expansionPanelToTag : ExpansionPanel -> TaggedExpansionPanel
expansionPanelToTag ep =
    (case ep of
        Projects ->
            "Projects"

        Labels ->
            "Labels"

        Filters ->
            "Filters"
    )
        |> tag


toggleTagged : Tagged tag comparable -> TaggedSet tag comparable -> TaggedSet tag comparable
toggleTagged tagged set =
    if Tagged.Set.member tagged set then
        Tagged.Set.remove tagged set

    else
        Tagged.Set.insert tagged set


toggleEP : ExpansionPanel -> ExpansionPanelSet -> ExpansionPanelSet
toggleEP item set =
    let
        taggedEP : Tagged ExpansionPanel String
        taggedEP =
            expansionPanelToTag item
    in
    toggleTagged taggedEP set


type Msg
    = TogglePanel ExpansionPanel


unwrap : Drawer -> ExpansionPanelSet
unwrap (Drawer epSet) =
    epSet


map : (ExpansionPanelSet -> ExpansionPanelSet) -> Drawer -> Drawer
map func =
    unwrap >> func >> Drawer


update : (Msg -> msg) -> Msg -> Drawer -> ( Drawer, Cmd msg )
update toMsg message model =
    case message of
        TogglePanel ep ->
            ( map (toggleEP ep) model, Cmd.none )


view toMsg { projectsCollapsed, toggleProjects } =
    [ navItem "Inbox"
    , navItem "Today"
    , navItem "Next 7 Days"
    , expansionPanel projectsCollapsed
        toggleProjects
        "Projects"
        [ subItem "FooBar"
        , subItem "Learn This"
        ]
    , expansionPanel projectsCollapsed
        toggleProjects
        "Projects"
        [ subItem "FooBar"
        , subItem "Learn This"
        ]
    ]


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
