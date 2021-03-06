module Dialog.UI exposing
    ( ActionsConfig
    , CheckboxConfig
    , FormContainer
    , InputConfig
    , checkbox
    , input
    , labeled
    , viewForm
    )

import Css
import Html.Styled as H exposing (Attribute, Html, button, div, label, span, text)
import Html.Styled.Attributes as A exposing (css, type_, value)
import Html.Styled.Events as E exposing (onClick, onInput, onSubmit)
import Key
import Style
import Styles exposing (..)
import Theme
import UI.Modal


type alias InputConfig msg =
    { labelText : String
    , value : String
    , changed : String -> msg
    , attrs : List (Attribute msg)
    }


input : InputConfig msg -> Html msg
input { labelText, value, changed, attrs } =
    label [ css [ Style.formGroup, flex, flexColumn ] ]
        [ span [ css [ Style.formLabel ] ] [ text labelText ]
        , H.input
            ([ css [ Style.formInput ]
             , A.value value
             , onInput changed
             ]
                ++ attrs
            )
            []
        ]


labeled : String -> Html msg -> Html msg
labeled labelText el =
    label [ css [ Style.formGroup, flex, flexColumn ] ]
        [ span [ css [ Style.formLabel ] ] [ text labelText ]
        , el
        ]


type alias CheckboxConfig msg =
    { labelText : String
    , value : Bool
    , changed : Bool -> msg
    }


checkbox : CheckboxConfig msg -> Html msg
checkbox { labelText, value, changed } =
    label [ css [ flex, itemsCenter, pv 2 ] ]
        [ div [ css [ pa 1 ] ]
            [ H.input
                [ css []
                , type_ "checkbox"
                , A.checked value
                , E.onCheck changed
                ]
                []
            ]
        , text labelText
        ]


type alias ActionsConfig msg =
    { submitTitle : String, canceled : msg, submitted : msg }


type alias FormContainer msg =
    { submit : msg
    , cancel : msg
    , title : String
    , submitTitle : String
    , content : List (Html msg)
    }


viewForm : FormContainer msg -> Html msg
viewForm { submit, submitTitle, cancel, title, content } =
    let
        headerContainer =
            div
                [ css
                    [ Css.fontSize Css.larger
                    , pa 3
                    , bo_b
                    , boColor <| Theme.borderGray
                    ]
                ]

        footerContainer =
            div [ css [ flex, flexRowReverse, pa 3, bo_t, boColor <| Theme.borderGray ] ]

        mainContainer =
            div [ css [ ph 3, Css.overflowY Css.auto ] ]
    in
    UI.Modal.container
        [ UI.Modal.overlay []
        , UI.Modal.form
            [ Key.onKeyDown [ Key.escape cancel ]
            , onSubmit submit
            ]
            [ headerContainer [ text title ]
            , mainContainer content
            , footerContainer
                [ btnSubmit submitTitle submit
                , span [ css [ hidden ] ] [ text "_" ]
                , btnCancel cancel
                ]
            ]
        ]


btnSubmit title action =
    button [ css [ Style.primaryBtn ], onClick action ] [ text title ]


btnCancel canceled =
    button [ css [ Style.btnLink ], onClick canceled ] [ text "Cancel" ]
