module TodoProject exposing
    ( TodoProject
    , fromMaybeProjectId
    , fromProject
    , fromTodo
    , inbox
    )

import Basics.More exposing (flip)
import Color exposing (Color)
import Project exposing (Project)
import ProjectCollection exposing (ProjectCollection)
import ProjectId exposing (ProjectId)
import Todo


type alias TodoProject =
    { id : Maybe ProjectId
    , title : String
    , color : Color
    }


fromProject : Project -> TodoProject
fromProject project =
    TodoProject (Just (Project.id project))
        (Project.title project)
        (Color.fromHSL ( toFloat <| Project.hue project, 70, 50 ))


inbox : TodoProject
inbox =
    TodoProject Nothing
        "Inbox"
        (Color.fromHSL ( 123, 70, 50 ))


fromMaybeProjectId : ProjectCollection -> Maybe ProjectId -> TodoProject
fromMaybeProjectId projectCollection =
    Maybe.andThen (flip ProjectCollection.byId projectCollection)
        >> Maybe.map fromProject
        >> Maybe.withDefault inbox


fromTodo : ProjectCollection -> Todo.Todo -> TodoProject
fromTodo projectCollection =
    Todo.maybeProjectId >> fromMaybeProjectId projectCollection
