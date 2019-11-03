module InboxOrProject exposing
    ( InboxOrProject
    , filterMap
    , filterMapProject
    , inbox
    , mapBoth
    , mapProject
    , project
    , toMaybeProject
    , unpack
    , unwrap
    )


type InboxOrProject inbox project
    = Inbox inbox
    | Project project


inbox : inbox -> InboxOrProject inbox project
inbox =
    Inbox


project : project -> InboxOrProject inbox project
project =
    Project


toMaybeProject : InboxOrProject () project -> Maybe project
toMaybeProject =
    unwrap Nothing Just


unpack : (inbox -> a) -> (project -> a) -> InboxOrProject inbox project -> a
unpack inboxFn projectFn model =
    case model of
        Inbox val ->
            inboxFn val

        Project val ->
            projectFn val


mapBoth : (inbox -> inbox2) -> (project -> project2) -> InboxOrProject inbox project -> InboxOrProject inbox2 project2
mapBoth inboxFn projectFn model =
    case model of
        Inbox val ->
            inboxFn val |> Inbox

        Project val ->
            projectFn val |> Project


unwrap : a -> (project -> a) -> InboxOrProject () project -> a
unwrap inboxVal =
    unpack (\() -> inboxVal)


mapProject : (project -> project) -> InboxOrProject inbox project -> InboxOrProject inbox project
mapProject projectFn model =
    case model of
        Project val ->
            projectFn val
                |> Project

        _ ->
            model


filterMapProject : (project -> Maybe project2) -> InboxOrProject inbox project -> Maybe (InboxOrProject inbox project2)
filterMapProject projectFn =
    filterMap Just projectFn


filterMap :
    (inbox -> Maybe inbox2)
    -> (project -> Maybe project2)
    -> InboxOrProject inbox project
    -> Maybe (InboxOrProject inbox2 project2)
filterMap inboxFn projectFn model =
    case model of
        Inbox val ->
            inboxFn val |> Maybe.map Inbox

        Project val ->
            projectFn val |> Maybe.map Project
