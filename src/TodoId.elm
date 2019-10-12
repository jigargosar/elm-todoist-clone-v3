module TodoId exposing (TodoId, decoder, toString)

import Json.Decode as JD exposing (Decoder)


type TodoId
    = TodoId String


toString : TodoId -> String
toString =
    unwrap


unwrap : TodoId -> String
unwrap (TodoId value) =
    value


fromStringDecoder : String -> Decoder TodoId
fromStringDecoder str =
    if str |> String.startsWith "TodoId-" then
        JD.succeed (TodoId str)

    else
        JD.fail ("Invalid Todo Id: " ++ str)


decoder : Decoder TodoId
decoder =
    JD.andThen fromStringDecoder JD.string
