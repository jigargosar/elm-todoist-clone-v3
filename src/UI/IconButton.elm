module UI.IconButton exposing (..)

import Html.Styled exposing (Html, button)
import Html.Styled.Events exposing (onClick)
import UI.Icon as Icon exposing (Icon)


view : Icon -> msg -> Html msg
view icon action =
    button [ onClick action ] [ Icon.view icon ]
