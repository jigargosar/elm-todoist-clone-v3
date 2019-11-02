module TodoProject exposing
    ( TodoProject
    , fromProject
    , fromProjectRef
    , fromTodo
    , href
    , inbox
    , notFound
    )

import CColor exposing (CColor)
import Color exposing (Color)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as A
import Project exposing (Project)
import ProjectCollection exposing (ProjectCollection)
import ProjectRef exposing (ProjectRef)
import Todo


type alias TodoProject =
    { ref : Maybe ProjectRef
    , title : String
    , color : Color
    }


fromProject : Project -> TodoProject
fromProject project =
    TodoProject (Just <| ProjectRef.fromId (Project.id project))
        (Project.title project)
        (Project.cColor project |> CColor.toColor)


inbox : TodoProject
inbox =
    TodoProject (Just ProjectRef.inbox)
        "Inbox"
        (CColor.toColor CColor.Charcoal)


notFound : TodoProject
notFound =
    TodoProject Nothing
        "not-found"
        (Color.fromHSL ( 0, 70, 50 ))


fromProjectRef : ProjectCollection -> ProjectRef -> TodoProject
fromProjectRef pc ref =
    case ProjectRef.id ref of
        Just projectId ->
            ProjectCollection.byId projectId pc
                |> Maybe.map fromProject
                |> Maybe.withDefault notFound

        Nothing ->
            inbox


fromTodo : ProjectCollection -> Todo.Todo -> TodoProject
fromTodo projectCollection =
    Todo.projectRef >> fromProjectRef projectCollection


href : { a | ref : Maybe ProjectRef.ProjectRef } -> Attribute msg
href =
    .ref >> Maybe.map ProjectRef.href >> Maybe.withDefault (A.href "")
