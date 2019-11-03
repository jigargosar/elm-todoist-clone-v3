module UI.IconButton exposing (..)

import Html.Styled exposing (Html, button)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Styles exposing (..)
import UI.Icon as Icon exposing (Icon)


view : Icon -> msg -> Html msg
view icon action =
    button [ css [ btnReset ], onClick action ] [ Icon.view icon ]
