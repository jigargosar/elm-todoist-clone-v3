module ProjectRef exposing (ProjectRef, fromId, href, id, inbox)

import Html.Styled exposing (Attribute)
import ProjectId
import Route


type ProjectRef
    = Inbox
    | ProjectId ProjectId.ProjectId


inbox : ProjectRef
inbox =
    Inbox


fromId : ProjectId.ProjectId -> ProjectRef
fromId =
    ProjectId


id : ProjectRef -> Maybe ProjectId.ProjectId
id model =
    case model of
        Inbox ->
            Nothing

        ProjectId projectId ->
            Just projectId


href : ProjectRef -> Attribute msg
href model =
    case model of
        Inbox ->
            Route.href Route.Inbox

        ProjectId projectId ->
            Route.projectIdHref projectId
