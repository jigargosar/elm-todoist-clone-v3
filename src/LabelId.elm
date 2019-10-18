module LabelId exposing (LabelId, decoder, toString, unique, uniqueListDecoder)

import Json.Decode as JD exposing (Decoder)
import Set


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


unique : List LabelId -> List LabelId
unique =
    List.map unwrap >> Set.fromList >> Set.toList >> List.map LabelId


uniqueListDecoder : Decoder (List LabelId)
uniqueListDecoder =
    JD.list decoder |> JD.map unique
