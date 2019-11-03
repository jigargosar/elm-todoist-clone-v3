module InboxOrProject exposing
    ( InboxOrProject
    , filterMap
    , filterMapProject
    , inbox
    , project
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


unpack : (inbox -> a) -> (project -> a) -> InboxOrProject inbox project -> a
unpack inboxFn projectFn model =
    case model of
        Inbox val ->
            inboxFn val

        Project val ->
            projectFn val


unwrap : a -> (project -> a) -> InboxOrProject () project -> a
unwrap inboxVal =
    unpack (\() -> inboxVal)


filterMapProject :
    (project -> Maybe project2)
    -> InboxOrProject inbox project
    -> Maybe (InboxOrProject inbox project2)
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
