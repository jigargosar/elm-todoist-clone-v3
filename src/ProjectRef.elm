module ProjectRef exposing (ProjectRef(..), fromId, inbox, toTodoProject)

import ProjectCollection
import ProjectId exposing (ProjectId)
import TodoProject


type ProjectRef
    = Inbox
    | ProjectId ProjectId


inbox : ProjectRef
inbox =
    Inbox


fromId : ProjectId -> ProjectRef
fromId =
    ProjectId


toTodoProject : ProjectCollection.ProjectCollection -> ProjectRef -> Maybe TodoProject.Model
toTodoProject pc model =
    case model of
        Inbox ->
            Just TodoProject.inbox

        ProjectId id ->
            ProjectCollection.byId id pc
                |> Maybe.map TodoProject.fromProject
