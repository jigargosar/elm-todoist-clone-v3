module Lens exposing (Config, System, compose, map, system, update)


type alias Lens small big =
    { get : big -> small
    , set : small -> big -> big
    }


type alias Config small big =
    { get : big -> small, set : small -> big -> big }


get : Lens small big -> big -> small
get =
    .get


set : Lens small big -> small -> big -> big
set =
    .set


map : Lens small big -> (small -> small) -> big -> big
map l fn big =
    set l (fn (get l big)) big


type alias System small big =
    { get : big -> small
    , set : small -> big -> big
    }


update : System small big -> (small -> ( small, other )) -> big -> ( big, other )
update bigL func big =
    func (bigL.get big)
        |> Tuple.mapFirst (\s -> bigL.set s big)


system : Config small big -> System small big
system =
    systemFromLens


compose : System medium big -> System small medium -> System small big
compose l1 l2 =
    system
        { get = l1.get >> l2.get
        , set = \s -> map l1 (l2.set s)
        }


systemFromLens : Lens small big -> System small big
systemFromLens l =
    { get = get l
    , set = set l
    }
