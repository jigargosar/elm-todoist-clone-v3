module Lens exposing (..)


type alias Lens s b =
    { get : b -> s, set : s -> b -> b }


fromTuple : ( b -> s, s -> b -> b ) -> Lens s b
fromTuple ( get, set ) =
    { get = get, set = set }


over : Lens s b -> (s -> s) -> b -> b
over lens func val =
    lens.set (func <| lens.get val) val


toggle : Lens Bool b -> b -> b
toggle lens =
    over lens not
