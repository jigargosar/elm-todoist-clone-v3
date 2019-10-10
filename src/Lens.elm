module Lens exposing (System, system)


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


type alias System small big =
    { get : big -> small
    , set : small -> big -> big
    , map : (small -> small) -> big -> big
    }


system : { get : big -> small, set : small -> big -> big } -> System small big
system =
    init >> systemFromLens


systemFromLens : Lens small big -> System small big
systemFromLens l =
    { get = get l
    , set = set l
    , map = map l
    }
