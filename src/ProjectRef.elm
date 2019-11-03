module ProjectRef exposing (ProjectRef, fromId, inbox, toTodoProject)

import InboxOrProject exposing (InboxOrProject)
import ProjectCollection exposing (ProjectCollection)
import ProjectId exposing (ProjectId)
import TodoProject


type alias ProjectRef =
    InboxOrProject () ProjectId


inbox : ProjectRef
inbox =
    InboxOrProject.inbox ()


fromId : ProjectId -> ProjectRef
fromId =
    InboxOrProject.project


toTodoProject : ProjectCollection -> ProjectRef -> Maybe TodoProject.Model
toTodoProject pc =
    InboxOrProject.unpack (\_ -> Just TodoProject.inbox)
        (\id ->
            ProjectCollection.byId id pc
                |> Maybe.map TodoProject.fromProject
        )
