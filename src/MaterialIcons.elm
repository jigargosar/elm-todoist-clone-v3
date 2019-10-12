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


done_all : Svg msg
done_all =
    fromUnstyled <| Material.Icons.Action.done_all defaultSize Inherit


inbox : Svg msg
inbox =
    fromUnstyled <| Material.Icons.Content.inbox defaultSize Inherit


schedule : Svg msg
schedule =
    fromUnstyled <| Material.Icons.Action.schedule defaultSize Inherit


calendar_today : Svg msg
calendar_today =
    fromUnstyled <| Material.Icons.Action.calendar_today defaultSize Inherit
