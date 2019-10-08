module TodoId exposing (TodoId, generator, toDictKey)

import Dict exposing (Dict)
import Random exposing (Generator)


type TodoId
    = TodoId String


generator : Generator TodoId
generator =
    Random.int 999 99999
        |> Random.map (String.fromInt >> (++) "TodoId-" >> TodoId)


toDictKey : TodoId -> String
toDictKey =
    unwrap


unwrap : TodoId -> String
unwrap (TodoId value) =
    value
