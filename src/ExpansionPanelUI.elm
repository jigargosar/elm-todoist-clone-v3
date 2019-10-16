module ExpansionPanelUI exposing (view, viewHeader)

import Css
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events exposing (onClick)
import Styles exposing (..)


iBtnStyle =
    batch [ btnReset, pointer ]


viewHeader : (Bool -> msg) -> String -> Bool -> Html msg
viewHeader toggle title isExpanded =
    let
        isCollapsed =
            not isExpanded
    in
    div
        [ css [ bo_b, boc (grayL 0.9), flex, hover [ bgGrayL 0.95 ] ] ]
        [ button
            [ css [ iBtnStyle, pa 1, flexGrow1 ], onClick (toggle (not isExpanded)) ]
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
        , button [ css [ iBtnStyle, mr 3 ] ] [ i [ class "material-icons" ] [ text "add" ] ]
        ]


view : (Bool -> msg) -> String -> (() -> List (Html msg)) -> Bool -> List (Html msg)
view toggle title lazyContent isExpanded =
    viewHeader toggle title isExpanded
        :: (if isExpanded then
                lazyContent ()

            else
                []
           )
