module ProjectRef exposing (ProjectRef, fromId, id, inbox)

import ProjectId


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
