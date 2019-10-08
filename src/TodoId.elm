module TodoId exposing (TodoId, generator, insert)

import Dict exposing (Dict)
import Random exposing (Generator)


type TodoId
    = TodoId String


generator : Generator TodoId
generator =
    Random.int 999 99999
        |> Random.map (String.fromInt >> (++) "TodoId-" >> TodoId)


insert : TodoId -> value -> Dict String value -> Dict String value
insert =
    Dict.insert << unwrap


unwrap : TodoId -> String
unwrap (TodoId value) =
    value
