module Lens exposing (Config, System, compose, system, update)

import Basics.More exposing (flip)


type Lens small big
    = Lens (Internal small big)


type alias Internal small big =
    { get : big -> small
    , set : small -> big -> big
    }


type alias Config small big =
    { get : big -> small, set : small -> big -> big }


init : Config small big -> Lens small big
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


setIn : Lens small big -> big -> small -> big
setIn =
    set >> flip


map : Lens small big -> (small -> small) -> big -> big
map l fn big =
    set l (fn (get l big)) big


type alias System small big =
    { get : big -> small
    , set : small -> big -> big
    , map : (small -> small) -> big -> big
    }


update : System small big -> (small -> ( small, other )) -> big -> ( big, other )
update bigL func big =
    func (bigL.get big)
        |> Tuple.mapFirst (\s -> bigL.set s big)


system : Config small big -> System small big
system =
    init >> systemFromLens


compose : System medium big -> System small medium -> System small big
compose l1 l2 =
    system
        { get = l1.get >> l2.get
        , set = \s -> l1.map (l2.set s)
        }


systemFromLens : Lens small big -> System small big
systemFromLens l =
    { get = get l
    , set = set l
    , map = map l
    }
