module TodoProject exposing
    ( fromProjectRef
    , view
    , viewProjectTitle
    )

import CColor exposing (CColor)
import Color exposing (Color)
import Css
import Html.Styled exposing (Attribute, Html, a, div, text)
import Html.Styled.Attributes as A exposing (css)
import Project exposing (Project)
import ProjectCollection exposing (ProjectCollection)
import ProjectId exposing (ProjectId)
import ProjectRef exposing (ProjectRef)
import Px
import Styles exposing (..)
import Todo exposing (Todo)
import UI.Icon as Icon
import UI.IconButton as IconButton


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


view : ProjectCollection -> Todo -> Html msg
view pc todo =
    let
        todoProject =
            fromTodo pc todo
    in
    a
        [ css
            [ linkReset
            , ph 1
            , lh 1.5
            , Css.fontSize Css.small
            , bg (toCssColor todoProject.color)
            , c_ (toCssColor <| Color.highContrast todoProject.color)
            , boRad 2
            , hover [ underline, pointer ]
            ]
        , href todoProject
        ]
        [ text todoProject.title ]


viewProjectTitle :
    { a | editClicked : ProjectId -> msg, noOp : msg }
    -> ProjectCollection
    -> ProjectRef
    -> Html msg
viewProjectTitle { editClicked, noOp } pc ref =
    let
        todoProject =
            fromProjectRef pc ref
    in
    div [ css [ flex, Px.pt 8 ] ]
        [ div
            [ css
                [ flexGrow1
                , Css.fontSize Css.large
                , bold
                , lh 1.5
                , Px.p2 8 8
                ]
            ]
            [ text todoProject.title ]
        , div [ css [ flex, selfCenter, Px.p2 0 8 ] ]
            [ todoProject.ref
                |> Maybe.andThen ProjectRef.id
                |> Maybe.map (editClicked >> IconButton.view Icon.Edit)
                |> Maybe.withDefault (text "")
            , IconButton.view Icon.Comment noOp
            , IconButton.view Icon.PersonAdd noOp
            , IconButton.view Icon.MoreHorizontal noOp
            ]
        ]
