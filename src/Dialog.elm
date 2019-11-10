module Dialog exposing
    ( Dialog
    , Msg
    , System
    , system
    )

-- DIALOG

import Browser.Dom as Dom
import Dialog.AddProject as AddProject exposing (AddProject)
import Dialog.EditProject as EditProject exposing (EditProject)
import Focus exposing (FocusResult)
import Html.Styled as H exposing (Html)
import Log
import Project exposing (Project)
import Ret exposing (Ret)


type alias System msg =
    { initial : Dialog
    , subscriptions : Dialog -> Sub msg
    , openAddProject : Int -> msg
    , openEditProject : Project -> msg
    , updateF : Msg -> Ret Dialog msg -> Ret Dialog msg
    , update : Msg -> Dialog -> Ret Dialog msg
    , view : Dialog -> Html msg
    }


system :
    { toMsg : Msg -> msg
    , projectAdded : AddProject.SavedWith -> msg
    , projectEdited : EditProject.SavedWith -> msg
    }
    -> System msg
system { toMsg, projectAdded, projectEdited } =
    let
        editProject : EditProject.System msg
        editProject =
            EditProject.system
                { toMsg = toMsg << SubMsg << EditProjectMsg
                , canceled = toMsg Canceled
                , saved = toMsg << SavedMsg << EditProjectSaved
                }

        update : Msg -> Dialog -> Ret Dialog msg
        update message model =
            case message of
                SubMsg subMsg ->
                    case ( subMsg, model ) of
                        ( AddProjectMsg msg, AddProject sub ) ->
                            AddProject.update addProjectConfig msg sub
                                |> Ret.mapBoth AddProject toMsg

                        ( EditProjectMsg msg, EditProject sub ) ->
                            editProject.update msg sub |> Ret.map EditProject

                        _ ->
                            Ret.only model

                OpenMsg msg ->
                    let
                        focusCmd =
                            Focus.cmd (toMsg << Focused)
                    in
                    case msg of
                        OpenAddProject idx ->
                            AddProject.initAt idx
                                |> Tuple.mapBoth AddProject focusCmd

                        OpenEditProject project ->
                            editProject.init project |> Ret.map EditProject

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
                                projectAdded savedWith

                            EditProjectSaved savedWith ->
                                projectEdited savedWith
                        )
                    )

        openMsg msg =
            toMsg << OpenMsg << msg
    in
    { initial = Closed
    , subscriptions = subscriptions toMsg
    , openAddProject = openMsg OpenAddProject
    , openEditProject = openMsg OpenEditProject
    , updateF = Ret.toUpdateF update
    , update = update
    , view = view toMsg
    }


addProjectConfig =
    AddProject.createConfig
        { toMsg = SubMsg << AddProjectMsg
        , canceled = Canceled
        , saved = SavedMsg << AddProjectSaved
        }


editProjectConfig =
    EditProject.createConfig
        { toMsg = SubMsg << EditProjectMsg
        , canceled = Canceled
        , saved = SavedMsg << EditProjectSaved
        }


type alias Config msg =
    { toMsg : Msg -> msg
    , addProject : AddProject.System msg
    , editProject : EditProject.System msg
    }


createConfig :
    { toMsg : Msg -> msg
    , projectAdded : AddProject.SavedWith -> msg
    , projectEdited : EditProject.SavedWith -> msg
    }
    -> Config msg
createConfig { toMsg, projectAdded, projectEdited } =
    let
        subMsg msg =
            toMsg << SubMsg << msg

        savedMsg msg =
            toMsg << SavedMsg << msg

        canceledMsg =
            toMsg Canceled

        editProject : EditProject.System msg
        editProject =
            EditProject.system
                { toMsg = subMsg EditProjectMsg
                , canceled = canceledMsg
                , saved = savedMsg EditProjectSaved
                }

        addProject =
            AddProject.system
                { toMsg = subMsg AddProjectMsg
                , canceled = canceledMsg
                , saved = savedMsg AddProjectSaved
                }
    in
    { toMsg = toMsg
    , editProject = editProject
    , addProject = addProject
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


subscriptions : (Msg -> msg) -> Dialog -> Sub msg
subscriptions toMsg dialog =
    (case dialog of
        AddProject model ->
            AddProject.subscriptions addProjectConfig model

        EditProject model ->
            EditProject.subscriptions editProjectConfig model

        Closed ->
            Sub.none
    )
        |> Sub.map toMsg


view : (Msg -> msg) -> Dialog -> Html msg
view toMsg dialog =
    (case dialog of
        AddProject model ->
            AddProject.view addProjectConfig model

        EditProject model ->
            EditProject.view editProjectConfig model

        Closed ->
            H.text ""
    )
        |> H.map toMsg
