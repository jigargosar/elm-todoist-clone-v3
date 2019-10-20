module Label exposing (Label, cssColor, decoder, hue, id, idx, setIdx, title)

import Css
import Json.Decode as JD exposing (Decoder)
import LabelId exposing (LabelId)
import Timestamp exposing (Timestamp)



-- MODEL


type Label
    = Label Internal


type alias Internal =
    { id : LabelId
    , createdAt : Timestamp
    , modifiedAt : Timestamp
    , title : String
    , idx : Int
    , hue : Int
    }


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    JD.map2 (|>)


decoder : Decoder Label
decoder =
    JD.succeed Internal
        |> andMap (JD.field "id" LabelId.decoder)
        |> andMap (JD.field "createdAt" Timestamp.decoder)
        |> andMap (JD.field "modifiedAt" Timestamp.decoder)
        |> andMap (JD.field "title" JD.string)
        |> andMap (JD.field "idx" JD.int)
        |> andMap (JD.field "hue" JD.int)
        |> JD.map Label


title : Label -> String
title =
    unwrap >> .title


hue : Label -> Int
hue =
    unwrap >> .hue


cssColor : Label -> Css.Color
cssColor label =
    Css.hsl (toFloat (hue label)) 0.7 0.5


unwrap (Label t) =
    t


id : Label -> LabelId
id =
    unwrap >> .id


idx : Label -> Int
idx =
    unwrap >> .idx



-- UPDATE


map : (Internal -> Internal) -> Label -> Label
map func =
    unwrap >> func >> Label


setIdx : Int -> Label -> Label
setIdx idx_ =
    map (\p -> { p | idx = idx_ })



-- VIEW
