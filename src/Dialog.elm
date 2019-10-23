module Dialog exposing (Dialog(..), container, dialogContent)

import Css
import FilterId exposing (FilterId)
import Html.Styled as H exposing (Html, div, text)
import Html.Styled.Attributes as A exposing (class, css)
import Html.Styled.Events as E
import Json.Decode as JD
import LabelId exposing (LabelId)
import Popper exposing (Popper)
import ProjectId exposing (ProjectId)
import Styles exposing (..)
import View exposing (View)


type Dialog
    = AddProject
    | EditProject ProjectId
    | AddLabel
    | EditLabel LabelId
    | AddFilter
    | EditFilter FilterId


container : { onClose : msg, noOp : msg } -> List (Html msg) -> View (Html msg)
container config content =
    View.portal
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
                    , pv 2
                    ]
                , A.id "rootPopup"
                , E.stopPropagationOn "click" (JD.succeed ( config.noOp, True ))
                , A.class "shadow-1"
                ]
                content
            ]
        ]


dialogContent : List (Html msg)
dialogContent =
    [ div [] [ text "Dialog Content" ] ]
