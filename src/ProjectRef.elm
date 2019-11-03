module ProjectRef exposing (ProjectRef(..), fromId, inbox)

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
