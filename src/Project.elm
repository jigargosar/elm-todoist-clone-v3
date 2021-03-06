module Project exposing (Project, cColor, cssColor, decoder, generator, id, idx, setCColor, setIdx, setModifiedAt, setTitle, title)

import CColor exposing (CColor)
import Css
import Json.Decode as JD exposing (Decoder)
import ProjectId exposing (ProjectId)
import Random
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
    , cColor : CColor
    }


generator : String -> Int -> CColor -> Timestamp -> Random.Generator Project
generator title_ idx_ cColor_ ts =
    ProjectId.generator
        |> Random.map
            (\id_ ->
                Internal id_ ts ts title_ idx_ cColor_
                    |> Project
            )


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
        |> andMap (JD.field "cColor" CColor.decoder)
        |> JD.map Project


title : Project -> String
title =
    unwrap >> .title


cssColor : Project -> Css.Color
cssColor =
    -- Css.hsl (toFloat (hue project)) 0.7 0.5
    cColor >> CColor.toCssColor


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


setTitle : String -> Project -> Project
setTitle title_ =
    map (\p -> { p | title = title_ })


setCColor : CColor -> Project -> Project
setCColor cColor_ =
    map (\p -> { p | cColor = cColor_ })


setModifiedAt : Timestamp -> Project -> Project
setModifiedAt modifiedAt_ =
    map (\p -> { p | modifiedAt = modifiedAt_ })



-- VIEW
