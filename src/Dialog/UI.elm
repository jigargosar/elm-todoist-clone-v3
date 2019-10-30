module Dialog.UI exposing (InputConfig, input)

import Html.Styled as H exposing (Attribute, Html, label, span, text)
import Html.Styled.Attributes as A exposing (css, value)
import Html.Styled.Events exposing (onInput)
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
