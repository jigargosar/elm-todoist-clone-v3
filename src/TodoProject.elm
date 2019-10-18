module TodoProject exposing
    ( TodoProject
    , fromMaybeProjectId
    , fromProject
    , fromTodo
    , inbox
    )

import Basics.More exposing (flip)
import Css
import Project exposing (Project)
import ProjectCollection exposing (ProjectCollection)
import ProjectId exposing (ProjectId)
import Todo


type alias TodoProject =
    { id : Maybe ProjectId
    , title : String
    , color : Css.Color
    }


fromProject : Project -> TodoProject
fromProject project =
    TodoProject (Just (Project.id project))
        (Project.title project)
        (Project.cssColor project)


inbox : TodoProject
inbox =
    TodoProject Nothing
        "Inbox"
        (Css.hsl 123 0.7 0.5)


fromMaybeProjectId : ProjectCollection -> Maybe ProjectId -> TodoProject
fromMaybeProjectId projectCollection =
    Maybe.andThen (flip ProjectCollection.byId projectCollection)
        >> Maybe.map fromProject
        >> Maybe.withDefault inbox


fromTodo : ProjectCollection -> Todo.Todo -> TodoProject
fromTodo projectCollection =
    Todo.maybeProjectId >> fromMaybeProjectId projectCollection
