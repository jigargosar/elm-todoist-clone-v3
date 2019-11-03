module TodoProjectV2 exposing (TodoProjectV2, fromRef)

import Project exposing (Project)
import ProjectCollection exposing (ProjectCollection)
import ProjectId exposing (ProjectId)
import ProjectRef exposing (ProjectRef)


type TodoProjectV2
    = Inbox
    | Project Project
    | NotFound ProjectId


fromRef : ProjectRef -> ProjectCollection -> TodoProjectV2
fromRef ref pc =
    case ref of
        ProjectRef.Inbox ->
            Inbox

        ProjectRef.ProjectId id ->
            ProjectCollection.byId id pc
                |> Maybe.map Project
                |> Maybe.withDefault (NotFound id)
