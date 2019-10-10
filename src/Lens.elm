module Lens exposing (Lens, get, init, set)


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
