module Drawer exposing (view)

import Css
import Html.Styled exposing (..)
import MaterialIcons as MI
import Styles exposing (..)


view =
    [ navItem "Inbox"
    , navItem "Today"
    , navItem "Next 7 Days"
    , navGroup "Projects"
    , subItem "FooBar"
    , subItem "Learn This"
    ]


subItem title =
    styled div
        [ pa 2, pointer ]
        []
        [ text title
        ]


navItem title =
    styled div [ pa 2, pointer ] [] [ text title ]


navGroup title =
    styled div
        [ flex, Css.hover [ bgGrayN 0.95 ] ]
        []
        [ iBtn [ flexGrow1 ] [] [ MI.expand_more, styled span [ bold, pa 1 ] [] [ text title ] ]
        , iBtn [] [] [ MI.add ]
        ]


iBtn styles =
    styled button (btnReset :: pa 1 :: pointer :: styles)
