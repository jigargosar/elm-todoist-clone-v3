module Dialog exposing (Dialog(..), addProjectContent, container, initAddProject, viewDialog)

import Css
import FilterId exposing (FilterId)
import Html.Styled exposing (Html, button, div, input, label, span, text)
import Html.Styled.Attributes as A exposing (css, type_, value)
import Html.Styled.Events exposing (onClick)
import LabelId exposing (LabelId)
import ProjectId exposing (ProjectId)
import Styles exposing (..)
import Theme
import View exposing (View)


type alias AddProjectState =
    { title : String
    , color : String
    }


initAddProject : Dialog
initAddProject =
    AddProject <| AddProjectState "" ""


type Dialog
    = AddProject AddProjectState
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


addProjectContent : Config msg -> AddProjectState -> View (Html msg)
addProjectContent config state =
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
            [ formTextIpt "Project name" state.title
            , formTextIpt "Project color" state.color
            , label [ css [ flex, itemsCenter, pv 2 ] ]
                [ div [ css [ pa 1 ] ] [ input [ css [], type_ "checkbox" ] [] ]
                , text "Add to favorites"
                ]
            ]
        , div [ css [ flex, flexRowReverse, pa 2, bo_t, boc <| Theme.borderGray ] ]
            [ button [ css [ plainBtn ], onClick config.cancel ] [ text "Add" ]
            , button
                [ css [ plainBtn ]
                , onClick config.cancel
                ]
                [ text "Cancel" ]
            ]
        ]


plainBtn =
    batch [ btnReset, pa 2 ]


ipt val =
    input
        [ css [ lh 1.5, pa 1, bo_a, boc <| Theme.borderGray ]
        , value val
        ]
        []


lbl title =
    span [ css [ lh 1.5, bold ] ] [ text title ]


formTextIpt title val =
    label [ css [ flex, flexColumn, pv 2 ] ]
        [ lbl title
        , ipt val
        ]


type alias Config msg =
    { cancel : msg }


viewDialog : Config msg -> Dialog -> View (Html msg)
viewDialog config dialog =
    case dialog of
        AddProject model ->
            container <|
                addProjectContent config model

        _ ->
            container View.none
