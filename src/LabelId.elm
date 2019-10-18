module LabelId exposing (LabelId, decoder, toString)

import Json.Decode as JD exposing (Decoder)


type LabelId
    = LabelId String


toString : LabelId -> String
toString =
    unwrap


unwrap : LabelId -> String
unwrap (LabelId value) =
    value


fromStringDecoder : String -> Decoder LabelId
fromStringDecoder str =
    if str |> String.startsWith "LabelId-" then
        JD.succeed (LabelId str)

    else
        JD.fail ("Invalid Label Id: " ++ str)


decoder : Decoder LabelId
decoder =
    JD.andThen fromStringDecoder JD.string
