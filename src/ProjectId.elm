module ProjectId exposing (ProjectId, decoder, toString)

import Json.Decode as JD exposing (Decoder)


type ProjectId
    = ProjectId String


toString : ProjectId -> String
toString =
    unwrap


unwrap : ProjectId -> String
unwrap (ProjectId value) =
    value


fromStringDecoder : String -> Decoder ProjectId
fromStringDecoder str =
    if str |> String.startsWith "ProjectId-" then
        JD.succeed (ProjectId str)

    else
        JD.fail ("Invalid Project Id: " ++ str)


decoder : Decoder ProjectId
decoder =
    JD.andThen fromStringDecoder JD.string
