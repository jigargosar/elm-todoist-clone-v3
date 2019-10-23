module Dialog exposing (Dialog(..), addProjectContent, container, dialogContent)

import Css
import FilterId exposing (FilterId)
import Html.Styled as H exposing (Html, div, input, label, text)
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


container : { onClose : msg, noOp : msg } -> View.Html msg -> View (Html msg)
container config content =
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
            , E.onClick config.onClose
            ]
            [ div
                [ css
                    [ Styles.bgWhite
                    , Styles.bor 3
                    ]
                , A.id ""
                , E.stopPropagationOn "click" (JD.succeed ( config.noOp, True ))
                , A.class "shadow-1"
                ]
                content.content
            ]
        ]
            ++ content.portal


dialogContent : View.Html msg
dialogContent =
    View.content [ div [] [ text "Dialog Content" ] ]


addProjectContent =
    View.content
        [ div [ css [ bold, pa 3, bo_b, boc <| Theme.borderGray ] ] [ text "Add Project" ]
        , div [ css [ pa 2 ] ]
            [ div [ css [ flex ] ]
                [ label [] [ text "Project name" ]
                , input [] []
                ]
            , div [ css [ flex ] ]
                [ label [] [ text "Project color" ]
                , input [] []
                ]
            , div [ css [ flex ] ]
                [ input [ type_ "checkbox" ] []
                , label [] [ text "Add to favorites" ]
                ]
            ]
        ]
