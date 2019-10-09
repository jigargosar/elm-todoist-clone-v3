module TodoId exposing (TodoId, decoder, generator, toDictKey)

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
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


decoder : Decoder TodoId
decoder =
    JD.string
        |> JD.andThen
            (\id ->
                if id |> String.startsWith "TodoId-" then
                    JD.succeed (TodoId id)

                else
                    JD.fail
                        ("Invalid Todo Id" ++ id)
            )
