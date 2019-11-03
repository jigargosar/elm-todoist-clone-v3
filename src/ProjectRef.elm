module ProjectRef exposing (ProjectRef, fromId, inbox, unwrap)

import InboxOrProject exposing (InboxOrProject)
import ProjectId exposing (ProjectId)


type alias ProjectRef =
    InboxOrProject () ProjectId


inbox : ProjectRef
inbox =
    InboxOrProject.inbox ()


fromId : ProjectId -> ProjectRef
fromId =
    InboxOrProject.project


unwrap : a -> (ProjectId -> a) -> ProjectRef -> a
unwrap =
    InboxOrProject.unwrap
