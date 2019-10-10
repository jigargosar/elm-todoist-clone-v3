module MaterialIcons exposing (..)

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events exposing (onClick)
import Material.Icons exposing (Coloring(..))
import Material.Icons.Action
import Material.Icons.Content
import Svg.Styled exposing (..)


search : Svg msg
search =
    fromUnstyled <| Material.Icons.Action.search 32 Inherit


add : Svg msg
add =
    fromUnstyled <| Material.Icons.Content.add 32 Inherit


buttonHelp : Maybe msg -> List (Html msg) -> Html msg
buttonHelp maybeMsg =
    Html.button
        [ class "select-none ma0 pa0 bn bg-inherit color-inherit flex"
        , Maybe.map onClick maybeMsg
            |> Maybe.withDefault (class "")
        ]
