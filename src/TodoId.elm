module TodoId exposing (TodoId, decoder, toDictKey)

import Json.Decode as JD exposing (Decoder)


type TodoId
    = TodoId String


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
