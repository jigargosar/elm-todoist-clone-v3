module Dialog exposing
    ( Config
    , Dialog
    , Msg
    , createConfig
    , initial
    , openAddProject
    , openEditProject
    , subscriptions
    , update
    , view
    )

-- DIALOG

import Basics.More exposing (msgToCmd)
import Dialog.AddProject as AddProject
import Dialog.EditProject as EditProject
import Html.Styled exposing (Html)
import Project exposing (Project)


type Dialog
    = AddProjectDialog AddProject.Model
    | EditProjectDialog EditProject.Model
      {- | AddLabelDialog
         | EditLabelDialog LabelId
         | AddFilterDialog
         | EditFilterDialog FilterId
      -}
    | Closed


type alias Config msg =
    { addProject : AddProject.Config msg
    , editProject : EditProject.Config msg
    , projectAdded : AddProject.SavedWith -> msg
    , projectEdited : EditProject.SavedWith -> msg
    }


createConfig :
    { toMsg : Msg -> msg
    , projectAdded : AddProject.SavedWith -> msg
    , projectEdited : EditProject.SavedWith -> msg
    }
    -> Config msg
createConfig c =
    let
        canceled =
            c.toMsg Canceled
    in
    { addProject =
        { toMsg = c.toMsg << AddProjectDialogMsg
        , canceled = canceled
        , saved = c.toMsg << SavedMsg << AddProjectSaved
        }
    , editProject =
        { toMsg = c.toMsg << EditProjectDialogMsg
        , canceled = canceled
        , saved = c.toMsg << SavedMsg << EditProjectSaved
        }
    , projectAdded = c.projectAdded
    , projectEdited = c.projectEdited
    }


type SavedMsg
    = AddProjectSaved AddProject.SavedWith
    | EditProjectSaved EditProject.SavedWith


type Msg
    = AddProjectDialogMsg AddProject.Msg
    | EditProjectDialogMsg EditProject.Msg
    | OpenAddProjectDialog Int
    | OpenEditProject Project
    | SavedMsg SavedMsg
    | Canceled


openAddProject : Int -> Msg
openAddProject =
    OpenAddProjectDialog


openEditProject : Project -> Msg
openEditProject =
    OpenEditProject


initial =
    Closed


subscriptions : Config msg -> Dialog -> Sub msg
subscriptions config dialog =
    case dialog of
        AddProjectDialog model ->
            AddProject.subscriptions config.addProject model

        EditProjectDialog model ->
            EditProject.subscriptions config.editProject model

        _ ->
            Sub.none


update : Config msg -> Msg -> Dialog -> ( Dialog, Cmd msg )
update config message dialogModel =
    let
        ret : ( Dialog, Cmd msg )
        ret =
            ( dialogModel, Cmd.none )
    in
    case message of
        AddProjectDialogMsg msg ->
            case dialogModel of
                AddProjectDialog model ->
                    AddProject.update config.addProject msg model
                        |> Tuple.mapFirst AddProjectDialog

                _ ->
                    ret

        EditProjectDialogMsg msg ->
            case dialogModel of
                EditProjectDialog model ->
                    EditProject.update config.editProject msg model
                        |> Tuple.mapFirst EditProjectDialog

                _ ->
                    ret

        OpenAddProjectDialog idx ->
            AddProject.initAt config.addProject idx
                |> Tuple.mapFirst AddProjectDialog

        OpenEditProject project ->
            EditProject.init config.editProject project
                |> Tuple.mapFirst EditProjectDialog

        Canceled ->
            ( Closed, Cmd.none )

        SavedMsg savedMsg ->
            case savedMsg of
                AddProjectSaved savedWith ->
                    ( Closed, config.projectAdded savedWith |> msgToCmd )

                EditProjectSaved savedWith ->
                    ( Closed, config.projectEdited savedWith |> msgToCmd )


view : Config msg -> Dialog -> List (Html msg)
view config dialog =
    case dialog of
        AddProjectDialog model ->
            [ AddProject.view config.addProject model ]

        EditProjectDialog model ->
            [ EditProject.view config.editProject model ]

        Closed ->
            []
