module InboxOrProject exposing (InboxOrProject, inbox, project, unpack)


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
