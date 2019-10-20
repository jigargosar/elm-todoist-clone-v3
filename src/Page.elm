module Page exposing (..)

import LabelId exposing (LabelId)
import ProjectRef exposing (ProjectRef)
import Route
import Url exposing (Url)


type Page
    = TodoListByProjectRef ProjectRef
    | TodoListByLabelId LabelId
    | NotFound Url


pageFromRoute : Route.Route -> Page
pageFromRoute route =
    case route of
        Route.Root ->
            TodoListByProjectRef ProjectRef.inbox

        Route.Inbox ->
            TodoListByProjectRef ProjectRef.inbox

        Route.Project projectId ->
            TodoListByProjectRef <| ProjectRef.fromId projectId

        Route.Label labelId ->
            TodoListByLabelId labelId


pageFromUrl : Url -> Page
pageFromUrl url =
    Route.fromUrl url |> Maybe.map pageFromRoute |> Maybe.withDefault (NotFound url)
