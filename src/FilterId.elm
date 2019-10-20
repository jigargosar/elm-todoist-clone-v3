module FilterId exposing (FilterId, decoder, toString)

import Json.Decode as JD exposing (Decoder)


type FilterId
    = FilterId String


toString : FilterId -> String
toString =
    unwrap


unwrap : FilterId -> String
unwrap (FilterId value) =
    value


fromStringDecoder : String -> Decoder FilterId
fromStringDecoder str =
    if str |> String.startsWith "FilterId-" then
        JD.succeed (FilterId str)

    else
        JD.fail ("Invalid Filter Id: " ++ str)


decoder : Decoder FilterId
decoder =
    JD.andThen fromStringDecoder JD.string
