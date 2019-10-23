module Dialog exposing (Dialog(..), addProjectContent, container, dialogContent)

import Css
import FilterId exposing (FilterId)
import Html.Styled as H exposing (Html, button, div, input, label, span, text)
import Html.Styled.Attributes as A exposing (class, css, type_)
import Html.Styled.Events as E
import Json.Decode as JD
import LabelId exposing (LabelId)
import ProjectId exposing (ProjectId)
import Styles exposing (..)
import Theme
import View exposing (View)


type Dialog
    = AddProject
    | EditProject ProjectId
    | AddLabel
    | EditLabel LabelId
    | AddFilter
    | EditFilter FilterId


container : View.Html msg -> View (Html msg)
container content =
    View.portal <|
        [ div
            [ css
                [ Styles.fixed
                , Styles.absFill
                , Styles.flex
                , Styles.itemsCenter
                , Styles.justifyCenter
                , Styles.bg (Css.hsla 0 0 0 0.2)

                --                 , Styles.bg (Css.hsla 0 1 1 0.6)
                , Styles.z_ 10
                ]
            ]
            [ div
                [ css
                    [ Styles.bgWhite
                    , Styles.bor 3
                    , w_ 300
                    , max_w_pct 100
                    ]
                , A.id ""
                , A.class "shadow-1"
                ]
                content.content
            ]
        ]
            ++ content.portal


dialogContent : View.Html msg
dialogContent =
    --View.content [ div [] [ text "Dialog Content" ] ]
    addProjectContent


addProjectContent : View (Html msg)
addProjectContent =
    View.content
        [ div
            [ css
                [ Css.fontSize Css.larger
                , pa 3
                , bo_b
                , boc <| Theme.borderGray
                ]
            ]
            [ text "Add Project" ]
        , div [ css [ ph 3 ] ]
            [ formTextIpt "Project name"
            , formTextIpt "Project color"
            , label [ css [ flex, itemsCenter, pv 2 ] ]
                [ div [ css [ pa 1 ] ] [ input [ css [], type_ "checkbox" ] [] ]
                , text "Add to favorites"
                ]
            ]
        , div [ css [ flex, flexRowReverse, pa 2, bo_t, boc <| Theme.borderGray ] ]
            [ button [ css [ plainBtn ] ] [ text "Add" ]
            , button [ css [ plainBtn ] ] [ text "Cancel" ]
            ]
        ]


plainBtn =
    batch [ btnReset, pa 2 ]


ipt =
    input [ css [ lh 1.5, pa 1, bo_a, boc <| Theme.borderGray ] ] []


lbl title =
    span [ css [ lh 1.5, bold ] ] [ text title ]


formTextIpt title =
    label [ css [ flex, flexColumn, pv 2 ] ]
        [ lbl title
        , ipt
        ]
