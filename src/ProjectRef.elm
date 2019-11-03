module ProjectRef exposing (ProjectRef(..), fromId, inbox)

import ProjectId exposing (ProjectId)


type ProjectRef
    = Inbox
    | ProjectId ProjectId


inbox : ProjectRef
inbox =
    Inbox


fromId : ProjectId -> ProjectRef
fromId =
    ProjectId
