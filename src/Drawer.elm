module Drawer exposing (view)

import Html.Styled exposing (..)
import Styles exposing (..)


view =
    [ navItem "Inbox"
    , navItem "Today"
    , navItem "Next 7 Days"
    , navGroup "Projects" [ subItem "FooBar", subItem "Learn This" ]
    ]


subItem title =
    styled div [ pa 2, pl 3, pointer ] [] [ text title ]


navItem title =
    styled div [ pa 2, pointer ] [] [ text title ]


navGroup title items =
    styled div
        []
        []
        (styled div [ ph 3, pv 2, ttu ] [] [ text title ] :: items)
