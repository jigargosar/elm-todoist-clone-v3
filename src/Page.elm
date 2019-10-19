module Page exposing (..)

import ProjectRef exposing (ProjectRef)
import Route
import Url exposing (Url)


type Page
    = TodoListByProjectRef ProjectRef
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


pageFromUrl : Url -> Page
pageFromUrl url =
    Route.fromUrl url |> Maybe.map pageFromRoute |> Maybe.withDefault (NotFound url)
