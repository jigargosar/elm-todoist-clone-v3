module Dialog exposing
    ( Config
    , Dialog
    , Msg
    , System
    , createConfig
    , subscriptions
    , system
    , update
    , view
    )

-- DIALOG

import Browser.Dom as Dom
import Dialog.AddProject as AddProject exposing (AddProject)
import Dialog.EditProject as EditProject exposing (EditProject)
import Focus exposing (FocusResult)
import Html.Styled as H exposing (Html)
import Log
import Project exposing (Project)
import Ret exposing (Ret, RetF)


type alias System msg =
    { initial : Dialog
    , subscriptions : Dialog -> Sub msg
    , openAddProject : Int -> msg
    , openEditProject : Project -> msg
    , update : Msg -> Dialog -> Ret Dialog msg
    , view : Dialog -> Html msg
    }


system :
    { toMsg : Msg -> msg
    , projectAdded : AddProject.SavedWith -> msg
    , projectEdited : EditProject.SavedWith -> msg
    }
    -> System msg
system ({ toMsg } as configParams) =
    let
        openMsg msg =
            toMsg << OpenMsg << msg

        config : Config msg
        config =
            createConfig configParams
    in
    { initial = Closed
    , openAddProject = openMsg OpenAddProject
    , openEditProject = openMsg OpenEditProject
    , subscriptions = subscriptions config
    , update = update config
    , view = view config
    }


type alias Config msg =
    { toMsg : Msg -> msg
    , focusCmd : String -> Cmd msg
    , projectAdded : AddProject.SavedWith -> msg
    , projectEdited : EditProject.SavedWith -> msg
    , apSys : AddProject.System msg
    , epSys : EditProject.System msg
    }


createConfig :
    { toMsg : Msg -> msg
    , projectAdded : AddProject.SavedWith -> msg
    , projectEdited : EditProject.SavedWith -> msg
    }
    -> Config msg
createConfig { toMsg, projectAdded, projectEdited } =
    { toMsg = toMsg
    , focusCmd = Focus.cmd (toMsg << Focused)
    , projectAdded = projectAdded
    , projectEdited = projectEdited
    , apSys =
        AddProject.system
            { toMsg = toMsg << SubMsg << AddProjectMsg
            , canceled = toMsg Canceled
            , saved = toMsg << SavedMsg << AddProjectSaved
            }
    , epSys =
        EditProject.system
            { toMsg = toMsg << SubMsg << EditProjectMsg
            , canceled = toMsg Canceled
            , saved = toMsg << SavedMsg << EditProjectSaved
            }
    }


type Dialog
    = AddProject AddProject
    | EditProject EditProject
      {- | AddLabelDialog
         | EditLabelDialog LabelId
         | AddFilterDialog
         | EditFilterDialog FilterId
      -}
    | Closed


type SavedMsg
    = AddProjectSaved AddProject.SavedWith
    | EditProjectSaved EditProject.SavedWith


type SubMsg
    = AddProjectMsg AddProject.Msg
    | EditProjectMsg EditProject.Msg


type OpenMsg
    = OpenAddProject Int
    | OpenEditProject Project


type Msg
    = SubMsg SubMsg
    | OpenMsg OpenMsg
    | SavedMsg SavedMsg
    | Canceled
    | Focused FocusResult


subscriptions : Config msg -> Dialog -> Sub msg
subscriptions c dialog =
    case dialog of
        AddProject model ->
            c.apSys.subscriptions model

        EditProject model ->
            c.epSys.subscriptions model

        Closed ->
            Sub.none


update : Config msg -> Msg -> Dialog -> Ret Dialog msg
update c message model =
    case message of
        SubMsg subMsg ->
            case ( subMsg, model ) of
                ( AddProjectMsg msg, AddProject sub ) ->
                    c.apSys.update msg sub |> Ret.map AddProject

                ( EditProjectMsg msg, EditProject sub ) ->
                    c.epSys.update msg sub |> Ret.map EditProject

                _ ->
                    Ret.only model

        OpenMsg msg ->
            let
                focusCmd =
                    Focus.cmd (c.toMsg << Focused)
            in
            case msg of
                OpenAddProject idx ->
                    c.apSys.initAt idx |> Tuple.mapBoth AddProject focusCmd

                OpenEditProject project ->
                    c.epSys.init project |> Tuple.mapBoth EditProject focusCmd

        Focused result ->
            case result of
                Err (Dom.NotFound domId) ->
                    ( model, Log.logError <| "dialog focus failed: " ++ domId )

                Ok () ->
                    ( model, Cmd.none )

        Canceled ->
            Ret.only Closed

        SavedMsg savedMsg ->
            ( Closed
            , Ret.toCmd
                (case savedMsg of
                    AddProjectSaved savedWith ->
                        c.projectAdded savedWith

                    EditProjectSaved savedWith ->
                        c.projectEdited savedWith
                )
            )


view : Config msg -> Dialog -> Html msg
view c dialog =
    case dialog of
        AddProject model ->
            c.apSys.view model

        EditProject model ->
            c.epSys.view model

        Closed ->
            H.text ""
