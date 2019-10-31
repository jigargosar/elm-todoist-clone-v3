module ProjectId exposing (ProjectId, decoder, generator, toString)

import Json.Decode as JD exposing (Decoder)
import Random


type ProjectId
    = ProjectId String


toString : ProjectId -> String
toString =
    unwrap


unwrap : ProjectId -> String
unwrap (ProjectId value) =
    value


prefix =
    "ProjectId-"


fromStringDecoder : String -> Decoder ProjectId
fromStringDecoder str =
    if str |> String.startsWith prefix then
        JD.succeed (ProjectId str)

    else
        JD.fail ("Invalid Project Id: " ++ str)


decoder : Decoder ProjectId
decoder =
    JD.andThen fromStringDecoder JD.string


generator : Random.Generator ProjectId
generator =
    Random.int 9999 99999
        |> Random.map (String.fromInt >> (++) prefix >> ProjectId)
