module Project exposing (Project, cColor, cssColor, decoder, hue, id, idx, setIdx, title)

import CColor exposing (CColor)
import Css
import Json.Decode as JD exposing (Decoder)
import ProjectId exposing (ProjectId)
import Timestamp exposing (Timestamp)



-- MODEL


type Project
    = Project Internal


type alias Internal =
    { id : ProjectId
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


decoder : Decoder Project
decoder =
    JD.succeed Internal
        |> andMap (JD.field "id" ProjectId.decoder)
        |> andMap (JD.field "createdAt" Timestamp.decoder)
        |> andMap (JD.field "modifiedAt" Timestamp.decoder)
        |> andMap (JD.field "title" JD.string)
        |> andMap (JD.field "idx" JD.int)
        |> andMap (JD.field "hue" JD.int)
        |> andMap (JD.field "cColor" CColor.decoder)
        |> JD.map Project


title : Project -> String
title =
    unwrap >> .title


hue : Project -> Int
hue =
    unwrap >> .hue


cssColor : Project -> Css.Color
cssColor project =
    Css.hsl (toFloat (hue project)) 0.7 0.5


cColor : Project -> CColor
cColor =
    unwrap >> .cColor


unwrap (Project t) =
    t


id : Project -> ProjectId
id =
    unwrap >> .id


idx : Project -> Int
idx =
    unwrap >> .idx



-- UPDATE


map : (Internal -> Internal) -> Project -> Project
map func =
    unwrap >> func >> Project


setIdx : Int -> Project -> Project
setIdx idx_ =
    map (\p -> { p | idx = idx_ })



-- VIEW
