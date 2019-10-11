module Drawer exposing (view)

import Css
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import MaterialIcons as MI
import Styles exposing (..)


view =
    [ navItem "Inbox"
    , navItem "Today"
    , navItem "Next 7 Days"
    , expansionPanel True
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


expansionPanel collapsed title content =
    div [] (expansionPanelHeader collapsed title :: content)


expansionPanelHeader collapsed title =
    div
        [ css [ pa 1, bo_b, boc (grayL 0.9), flex, hover [ bgGrayL 0.95 ] ] ]
        [ button
            [ css [ iBtnStyle, flexGrow1 ] ]
            [ span
                [ css
                    [ c_grayL 0.6
                    , batch
                        [ styleIf collapsed [ Css.transforms [ Css.rotate (Css.deg 90) ] ]
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
