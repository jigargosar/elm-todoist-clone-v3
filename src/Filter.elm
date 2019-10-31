module Filter exposing (Filter, cssColor, decoder, hue, id, idx, setIdx, title)

import CColor exposing (CColor)
import Css
import FilterId exposing (FilterId)
import Json.Decode as JD exposing (Decoder)
import Timestamp exposing (Timestamp)



-- MODEL


type Filter
    = Filter Internal


type alias Internal =
    { id : FilterId
    , createdAt : Timestamp
    , modifiedAt : Timestamp
    , title : String
    , idx : Int
    , hue : Int
    , cColor : CColor
    }


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    JD.map2 (|>)


decoder : Decoder Filter
decoder =
    JD.succeed Internal
        |> andMap (JD.field "id" FilterId.decoder)
        |> andMap (JD.field "createdAt" Timestamp.decoder)
        |> andMap (JD.field "modifiedAt" Timestamp.decoder)
        |> andMap (JD.field "title" JD.string)
        |> andMap (JD.field "idx" JD.int)
        |> andMap (JD.field "hue" JD.int)
        |> andMap (JD.field "cColor" CColor.decoder)
        |> JD.map Filter


title : Filter -> String
title =
    unwrap >> .title


hue : Filter -> Int
hue =
    unwrap >> .hue


cssColor : Filter -> Css.Color
cssColor filter =
    Css.hsl (toFloat (hue filter)) 0.7 0.5


unwrap (Filter t) =
    t


id : Filter -> FilterId
id =
    unwrap >> .id


idx : Filter -> Int
idx =
    unwrap >> .idx



-- UPDATE


map : (Internal -> Internal) -> Filter -> Filter
map func =
    unwrap >> func >> Filter


setIdx : Int -> Filter -> Filter
setIdx idx_ =
    map (\p -> { p | idx = idx_ })



-- VIEW
