module Drawer exposing (view)

import Css exposing (transform, transforms)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import MaterialIcons as MI
import Styles exposing (..)


view { projectsCollapsed, toggleProjects } =
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
