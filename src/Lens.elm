module Lens exposing (Lens, compose, map, system, update)


type alias Lens small big =
    { get : big -> small
    , set : small -> big -> big
    }


map : Lens small big -> (small -> small) -> big -> big
map l fn big =
    l.set (fn (l.get big)) big


update : Lens small big -> (small -> ( small, other )) -> big -> ( big, other )
update bigL func big =
    func (bigL.get big)
        |> Tuple.mapFirst (\s -> bigL.set s big)


compose : Lens medium big -> Lens small medium -> Lens small big
compose l1 l2 =
    system
        { get = l1.get >> l2.get
        , set = \s -> map l1 (l2.set s)
        }


system : Lens small big -> Lens small big
system l =
    l
