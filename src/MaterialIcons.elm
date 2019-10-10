module MaterialIcons exposing (..)

import Material.Icons exposing (Coloring(..))
import Material.Icons.Action
import Material.Icons.Content
import Material.Icons.Navigation
import Svg.Styled exposing (Svg, fromUnstyled)


search : Svg msg
search =
    fromUnstyled <| Material.Icons.Action.search 32 Inherit


add : Svg msg
add =
    fromUnstyled <| Material.Icons.Content.add 32 Inherit


menu : Svg msg
menu =
    fromUnstyled <| Material.Icons.Navigation.menu 32 Inherit
