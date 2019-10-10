module Lens exposing (Lens, get, init, map, set)


type Lens small big
    = Lens (Internal small big)


type alias Internal small big =
    { get : big -> small
    , set : small -> big -> big
    }


init : { get : big -> small, set : small -> big -> big } -> Lens small big
init =
    Lens


unwrap : Lens small big -> Internal small big
unwrap (Lens m) =
    m


get : Lens small big -> big -> small
get =
    unwrap >> .get


set : Lens small big -> small -> big -> big
set =
    unwrap >> .set


map : Lens small big -> (small -> small) -> big -> big
map l fn big =
    set l (fn (get l big)) big
