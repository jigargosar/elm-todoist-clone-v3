module Drawer exposing (Panel, view)

import Css
import DnD exposing (DnD)
import Drag exposing (Drag)
import ExpansionPanelUI
import Html.Styled as H exposing (..)
import Html.Styled.Attributes as A exposing (class, css)
import Lens exposing (Lens)
import Project exposing (Project)
import ProjectId
import Return
import SelectList
import Styles exposing (..)
import Task



--


type alias Internal =
    { labelList : List LabelView
    , filterList : List FilterView
    }


labelList =
    [ LabelView "to read" 333
    , LabelView "medical" 93990
    , LabelView "quick-ref" 444
    ]


filterList =
    [ FilterView "Assigned to me" 933
    , FilterView "Assigned to others" 9354
    , FilterView "Priority 1" 93344
    , FilterView "Priority 2" 932323
    , FilterView "Priority 3" 932323
    , FilterView "View all" 932325
    , FilterView "No due date" 9355
    ]


type Panel
    = Projects
    | Labels
    | Filters


type alias LabelView =
    { title : String, hue : Float }


type alias FilterView =
    { title : String, hue : Float }


view : { toggleExpansionPanel : Panel -> msg } -> List Project -> { content : List (Html msg), portal : List (Html msg) }
view { toggleExpansionPanel } projectList =
    { content =
        [ navIconItem "Inbox" "inbox"
        , navIconItem "Today" "calendar_today"
        , navIconItem "Next 7 Days" "view_week"
        ]
            ++ ExpansionPanelUI.view (toggleExpansionPanel Projects)
                "Projects"
                (\_ ->
                    projectList |> List.map navProjectItem
                )
                True
            ++ ExpansionPanelUI.view (toggleExpansionPanel Labels)
                "Labels"
                (\_ ->
                    labelList |> List.map navLabelItem
                )
                True
            ++ ExpansionPanelUI.view (toggleExpansionPanel Filters)
                "Filters"
                (\_ ->
                    filterList
                        |> List.map navFilterItem
                )
                True
    , portal = []
    }


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


viewItem attributes styles title iconColor iconName =
    div
        (css
            [ ph 1
            , pointer
            , flex
            , c_grayL 0.3
            , batch styles
            ]
            :: attributes
        )
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


navProjectItem : Project -> Html msg
navProjectItem project =
    let
        title =
            Project.title project

        iconColor =
            Css.hsl (Project.hue project |> toFloat) 0.7 0.5
    in
    viewItem [] [] title iconColor "folder"


navLabelItem : LabelView -> Html msg
navLabelItem { title, hue } =
    viewItem [] [] title (Css.hsl hue 0.7 0.5) "label"


navFilterItem : FilterView -> Html msg
navFilterItem { title, hue } =
    viewItem [] [] title (Css.hsl hue 0.7 0.5) "filter_list"
