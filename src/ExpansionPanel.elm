module ExpansionPanel exposing (ExpansionPanel, initial, update)


type ExpansionPanel
    = ExpansionPanel Internal


type alias Internal =
    { collapsed : Bool }


initial =
    Internal False |> ExpansionPanel


type Msg
    = Toggle


unwrap (ExpansionPanel internal) =
    internal


map func =
    unwrap >> func >> ExpansionPanel


update : (Msg -> msg) -> Msg -> ExpansionPanel -> ( ExpansionPanel, Cmd msg )
update toMsg message model =
    case message of
        Toggle ->
            ( map (\i -> { i | collapsed = not i.collapsed }) model, Cmd.none )
