module Todo exposing
    ( Todo
    , decoder
    , id
    , idx
    , isCompleted
    , labelIdList
    , mapCompleted
    , projectRef
    , title
    , toggle
    )

import Json.Decode as JD exposing (Decoder)
import LabelId exposing (LabelId)
import ProjectId exposing (ProjectId)
import ProjectRef exposing (ProjectRef)
import Timestamp exposing (Timestamp)
import TodoId exposing (TodoId)



-- MODEL


type Todo
    = Todo Internal


type alias Internal =
    { id : TodoId
    , createdAt : Timestamp
    , modifiedAt : Timestamp
    , title : String
    , isCompleted : Bool
    , idx : Int
    , maybeProjectId : ProjectRef
    , labelIdList : List LabelId
    }


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    JD.map2 (|>)


decoder : Decoder Todo
decoder =
    JD.succeed Internal
        |> andMap (JD.field "id" TodoId.decoder)
        |> andMap (JD.field "createdAt" Timestamp.decoder)
        |> andMap (JD.field "modifiedAt" Timestamp.decoder)
        |> andMap (JD.field "title" JD.string)
        |> andMap (JD.field "isCompleted" JD.bool)
        |> andMap (JD.field "idx" JD.int)
        |> andMap (JD.field "maybeProjectId" projectRefDecoder)
        |> andMap (JD.field "labelIdList" LabelId.uniqueListDecoder)
        |> JD.map Todo


maybeProjectIdDecoder : Decoder (Maybe ProjectId)
maybeProjectIdDecoder =
    JD.oneOf [ JD.null Nothing, JD.map Just ProjectId.decoder ]


projectRefDecoder : Decoder ProjectRef
projectRefDecoder =
    JD.oneOf [ JD.null ProjectRef.inbox, JD.map ProjectRef.fromId ProjectId.decoder ]


title : Todo -> String
title =
    unwrap >> .title


unwrap (Todo t) =
    t


isCompleted : Todo -> Bool
isCompleted =
    unwrap >> .isCompleted


id : Todo -> TodoId
id =
    unwrap >> .id


idx : Todo -> Int
idx =
    unwrap >> .idx


projectRef : Todo -> ProjectRef.ProjectRef
projectRef =
    unwrap >> .maybeProjectId


labelIdList : Todo -> List LabelId
labelIdList =
    unwrap >> .labelIdList



-- UPDATE


map : (Internal -> Internal) -> Todo -> Todo
map func =
    unwrap >> func >> Todo


mapCompleted : (Bool -> Bool) -> Todo -> Todo
mapCompleted func =
    map (\t -> { t | isCompleted = func t.isCompleted })


toggle : Todo -> Todo
toggle =
    mapCompleted not



-- VIEW
