module Dialog.UI exposing (ActionsConfig, CheckboxConfig, ContainerConfig, InputConfig, actions, checkbox, container, input)

import Css
import Html.Styled as H exposing (Attribute, Html, button, div, label, span, text)
import Html.Styled.Attributes as A exposing (css, type_, value)
import Html.Styled.Events as E exposing (onClick, onInput, onSubmit)
import Key
import Px as PX
import Styles exposing (..)
import Theme


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
            ([ css [ lh 1.5, pa 1, bo_a, boc <| Theme.borderGray ]
             , A.value value
             , onInput changed
             ]
                ++ attrs
            )
            []
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


actions : ActionsConfig msg -> Html msg
actions { submitTitle, canceled, submitted } =
    div [ css [ flex, flexRowReverse, PX.p2 12 12, bo_t, boc <| Theme.borderGray ] ]
        [ btnSubmit "Add" submitted
        , btnCancel canceled
        ]


type alias ContainerConfig msg =
    { submitted : msg, canceled : msg, title : String }


container : ContainerConfig msg -> List (Html msg) -> List (Html msg)
container { submitted, canceled, title } children =
    let
        formAttrs =
            [ css
                [ bgWhite
                , Styles.bor 3
                , w_ 300
                , max_w_pct 100
                ]
            , A.class "shadow-1"
            , Key.onKeyDown [ Key.escape canceled ]
            , onSubmit submitted
            ]
    in
    [ div
        [ css
            [ Css.fontSize Css.larger
            , pa 3
            , bo_b
            , boc <| Theme.borderGray
            ]
        ]
        [ text title ]
    , div [ css [ ph 3 ] ] children
    ]


btnSubmit title action =
    button [ css [ plainBtnStyles ], onClick action ] [ text title ]


btnCancel canceled =
    button [ css [ plainBtnStyles ], onClick canceled ] [ text "Cancel" ]


plainBtnStyles =
    batch
        [ btnReset
        , PX.p2 4 8
        , bor 1
        , hover [ bgGrayL 0.95 ]
        , focus [ bgGrayL 0.9, z_ 1 ]
        ]
