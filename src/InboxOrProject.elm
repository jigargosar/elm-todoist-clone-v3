module InboxOrProject exposing
    ( InboxOrProject
    , filterMap
    , inbox
    , project
    , unpack
    , unwrap
    )


type InboxOrProject project
    = Inbox
    | Project project


inbox : InboxOrProject project
inbox =
    Inbox


project : project -> InboxOrProject project
project =
    Project


unpack : (() -> a) -> (project -> a) -> InboxOrProject project -> a
unpack inboxFn projectFn model =
    case model of
        Inbox ->
            inboxFn ()

        Project val ->
            projectFn val


unwrap : a -> (project -> a) -> InboxOrProject project -> a
unwrap inboxVal =
    unpack (\() -> inboxVal)


filterMap :
    (project -> Maybe project2)
    -> InboxOrProject project
    -> Maybe (InboxOrProject project2)
filterMap projectFn model =
    case model of
        Inbox ->
            Just Inbox

        Project val ->
            projectFn val |> Maybe.map Project
