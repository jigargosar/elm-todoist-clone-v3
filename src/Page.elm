module Page exposing (..)

import FilterId exposing (FilterId)
import LabelId exposing (LabelId)
import ProjectRef exposing (ProjectRef)
import Route
import Url exposing (Url)


type Page
    = TodoListByProjectRef ProjectRef
    | TodoListByLabelId LabelId
    | TodoListByFilterId FilterId
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

        Route.Filter filterId ->
            TodoListByFilterId filterId


pageFromUrl : Url -> Page
pageFromUrl url =
    Route.fromUrl url |> Maybe.map pageFromRoute |> Maybe.withDefault (NotFound url)
