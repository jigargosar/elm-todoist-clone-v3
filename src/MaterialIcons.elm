module MaterialIcons exposing (..)

import Material.Icons exposing (Coloring(..))
import Material.Icons.Action
import Material.Icons.Content
import Material.Icons.Navigation
import Svg.Styled exposing (Svg, fromUnstyled)


defaultSize =
    24


search : Svg msg
search =
    fromUnstyled <| Material.Icons.Action.search defaultSize Inherit


add : Svg msg
add =
    fromUnstyled <| Material.Icons.Content.add defaultSize Inherit


menu : Svg msg
menu =
    fromUnstyled <| Material.Icons.Navigation.menu defaultSize Inherit


arrow_drop_down : Svg msg
arrow_drop_down =
    fromUnstyled <| Material.Icons.Navigation.arrow_drop_down defaultSize Inherit


expand_more : Svg msg
expand_more =
    fromUnstyled <| Material.Icons.Navigation.expand_more defaultSize Inherit
