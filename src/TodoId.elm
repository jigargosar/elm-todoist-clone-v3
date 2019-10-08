module TodoId exposing (TodoId, generator)

import Random exposing (Generator)


type TodoId
    = TodoId String


generator : Generator TodoId
generator =
    Random.int 999 99999
        |> Random.map (String.fromInt >> (++) "TodoId-" >> TodoId)
