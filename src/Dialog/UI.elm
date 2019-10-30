module Dialog.UI exposing (CheckboxConfig, InputConfig, checkbox, input)

import Html.Styled as H exposing (Attribute, Html, div, label, span, text)
import Html.Styled.Attributes as A exposing (css, type_, value)
import Html.Styled.Events as E exposing (onInput)
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
