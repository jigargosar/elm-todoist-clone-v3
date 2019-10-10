module Lens exposing (Lens, init)


type Lens small big
    = Lens
        { get : big -> small
        , set : small -> big -> big
        }


init : { get : big -> small, set : small -> big -> big } -> Lens small big
init =
    Lens
