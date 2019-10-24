module ExpansionPanelUI exposing (Config, viewHeader)

import Css
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events as E exposing (onClick)
import Styles exposing (..)


iBtnStyle =
    batch [ btnReset, pointer ]


type alias Config msg =
    { toggle : msg
    , add : msg
    }


viewHeader : Config msg -> String -> Bool -> Html msg
viewHeader { toggle, add } title isExpanded =
    let
        isCollapsed =
            not isExpanded
    in
    div
        [ css [ bo_b, boc (grayL 0.9), flex, hover [ bgGrayL 0.95 ] ] ]
        [ button
            [ css [ iBtnStyle, pa 1, flexGrow1 ], onClick toggle ]
            [ span
                [ css
                    [ c_grayL 0.6
                    , batch
                        [ styleIf isCollapsed [ Css.transforms [ Css.rotate (Css.deg -90) ] ]
                        , transition [ Transitions.transform 200 ]
                        ]
                    ]
                ]
                [ i [ class "material-icons" ] [ text "expand_more" ] ]
            , styled span [ bold, pa 1 ] [] [ text title ]
            ]
        , button
            [ css [ iBtnStyle, mr 3 ]
            , E.onClick add
            ]
            [ i [ class "material-icons" ] [ text "add" ] ]
        ]
