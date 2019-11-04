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
import Html.Styled.Attributes as A exposing (css, for, type_, value)
import Html.Styled.Events as E exposing (onClick, onInput, onSubmit)
import Key
import Px as PX
import Styles exposing (..)
import Theme
import UI.Btn


type alias InputConfig msg =
    { labelText : String
    , value : String
    , changed : String -> msg
    , attrs : List (Attribute msg)
    }


input : InputConfig msg -> Html msg
input { labelText, value, changed, attrs } =
    label [ css [ flex, flexColumn, pv 2 ] ]
        [ span [ css [ lh 1.5, bold ] ] [ text labelText ]
        , H.input
            ([ css [ lh 1.5, pa 1, boAll, boColor <| Theme.borderGray ]
             , A.value value
             , onInput changed
             ]
                ++ attrs
            )
            []
        ]


labeled : String -> Html msg -> Html msg
labeled labelText el =
    label [ css [ flex, flexColumn, pv 2 ] ]
        [ span [ css [ lh 1.5, bold ] ] [ text labelText ]
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
        formAttrs =
            [ css
                [ bgWhite
                , Styles.boRad 3
                , w_ 300
                , max_w_pct 100
                ]
            , A.class "shadow-1"
            , Key.onKeyDown [ Key.escape cancel ]
            , onSubmit submit
            ]

        header =
            div
                [ css
                    [ Css.fontSize Css.larger
                    , pa 3
                    , bo_b
                    , boColor <| Theme.borderGray
                    ]
                ]
                [ text title ]

        formContent =
            [ header
            , div [ css [ ph 3 ] ] content
            , footer
            ]

        footer =
            div [ css [ flex, flexRowReverse, PX.p2 12 12, bo_t, boColor <| Theme.borderGray ] ]
                [ btnSubmit submitTitle submit
                , span [ css [ hidden ] ] [ text "_" ]
                , btnCancel cancel
                ]
    in
    div [ css [ overlayStyles ] ]
        [ H.form formAttrs formContent ]


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


btnSubmit title action =
    button [ css [ UI.Btn.style ], onClick action ] [ text title ]


btnCancel canceled =
    button [ css [ UI.Btn.linkStyle ], onClick canceled ] [ text "Cancel" ]
