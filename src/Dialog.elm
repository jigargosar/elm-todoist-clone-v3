module Dialog exposing
    ( Config
    , Dialog
    , DialogMsg
    , close
    , createConfig
    , initial
    , openAddProject
    , openEditProject
    , subscriptions
    , update
    , viewDialog
    )

-- DIALOG

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
    }


createConfig :
    { toMsg : DialogMsg -> msg
    , canceled : msg
    , projectAdded : AddProject.SavedWith -> msg
    , projectEdited : EditProject.SavedWith -> msg
    }
    -> Config msg
createConfig c =
    { addProject =
        { toMsg = c.toMsg << AddProjectDialogMsg
        , canceled = c.canceled
        , saved = c.projectAdded
        }
    , editProject =
        { toMsg = c.toMsg << EditProjectDialogMsg
        , canceled = c.canceled
        , saved = c.projectEdited
        }
    }


type DialogMsg
    = AddProjectDialogMsg AddProject.Msg
    | EditProjectDialogMsg EditProject.Msg
    | OpenAddProjectDialog Int
    | OpenEditProject Project
    | Close


openAddProject : Int -> DialogMsg
openAddProject =
    OpenAddProjectDialog


openEditProject : Project -> DialogMsg
openEditProject =
    OpenEditProject


initial =
    Closed


close : DialogMsg
close =
    Close


subscriptions : Config msg -> Dialog -> Sub msg
subscriptions config dialog =
    case dialog of
        AddProjectDialog model ->
            AddProject.subscriptions config.addProject model

        EditProjectDialog model ->
            EditProject.subscriptions config.editProject model

        _ ->
            Sub.none


update : Config msg -> DialogMsg -> Dialog -> ( Dialog, Cmd msg )
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

        Close ->
            ( Closed, Cmd.none )


viewDialog : Config msg -> Dialog -> List (Html msg)
viewDialog config dialog =
    case dialog of
        AddProjectDialog model ->
            [ AddProject.view config.addProject model ]

        EditProjectDialog model ->
            [ EditProject.view config.editProject model ]

        Closed ->
            []
