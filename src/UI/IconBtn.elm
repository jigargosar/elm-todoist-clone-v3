module UI.IconBtn exposing (..)

import Html.Styled exposing (Html, button)
import UI.Icon as Icon exposing (Icon)


view : Icon -> msg -> Html msg
view icon action =
    button [] [ Icon.view icon ]
