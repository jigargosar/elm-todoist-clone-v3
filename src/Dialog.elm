module Dialog exposing (Dialog(..), addProjectContent, initAddProject, viewDialog)

import Css
import FilterId exposing (FilterId)
import Html.Styled exposing (Attribute, Html, button, div, form, input, label, span, text)
import Html.Styled.Attributes as A exposing (css, type_, value)
import Html.Styled.Events as E exposing (onClick, onSubmit)
import Json.Decode as JD
import Key
import LabelId exposing (LabelId)
import ProjectId exposing (ProjectId)
import StyleAttrs as SA exposing (StyleAttrs)
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


overlayStyles =
    batch
        [ fixed
        , absFill
        , flex
        , itemsCenter
        , justifyCenter
        , bg (Css.hsla 0 0 0 0.2)

        --                 , bg (Css.hsla 0 1 1 0.6)
        , z_ 10
        ]


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
            [ btnSubmit "Add"
            , btnSubmit "Cancel"
            ]
        ]


btnSubmit title =
    button [ css [ plainBtnStyles ] ] [ text title ]


plainBtnStyles =
    batch [ btnReset, pv 1, ph 2, ma 1, hover [ bgGrayL 0.95 ], focus [ bgGrayL 0.8 ] ]


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
    let
        formAttrs =
            [ css
                [ bgWhite
                , Styles.bor 3
                , w_ 300
                , max_w_pct 100
                ]
            , A.class "shadow-1"
            , Key.onKeyDown [ Key.escape config.cancel ]
            , onSubmit config.cancel
            ]

        viewHelp innerView =
            { content =
                [ div [ css [ overlayStyles ] ]
                    [ form formAttrs innerView.content ]
                ]
            , portal = innerView.portal
            }
    in
    case dialog of
        AddProject model ->
            viewHelp <|
                addProjectContent config model

        _ ->
            View.none
