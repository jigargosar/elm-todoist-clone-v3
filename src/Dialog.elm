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
    , view : Dialog -> List (Html msg)
    }


system :
    { toMsg : Msg -> msg
    , projectAdded : AddProject.SavedWith -> msg
    , projectEdited : EditProject.SavedWith -> msg
    }
    -> System msg
system c =
    let
        toMsg =
            c.toMsg

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
                            Focus.cmd (c.toMsg << Focused)
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
                                c.projectAdded savedWith

                            EditProjectSaved savedWith ->
                                c.projectEdited savedWith
                        )
                    )

        view : Dialog -> List (Html msg)
        view dialog =
            case dialog of
                AddProject model ->
                    [ AddProject.view addProjectConfig model |> H.map toMsg ]

                EditProject model ->
                    [ editProject.view model ]

                Closed ->
                    []

        openMsg msg =
            c.toMsg << OpenMsg << msg
    in
    { initial = Closed
    , subscriptions = subscriptions toMsg
    , openAddProject = openMsg OpenAddProject
    , openEditProject = openMsg OpenEditProject
    , updateF = Ret.toUpdateF update
    , update = update
    , view = view
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
    case dialog of
        AddProject model ->
            AddProject.subscriptions addProjectConfig model |> Sub.map toMsg

        EditProject model ->
            EditProject.subscriptions editProjectConfig model |> Sub.map toMsg

        Closed ->
            Sub.none
