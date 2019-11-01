module Dialog exposing (..)

-- DIALOG

import Dialog.AddProject
import Dialog.EditProject
import FilterId exposing (FilterId)
import Html.Styled exposing (Html)
import LabelId exposing (LabelId)
import Project exposing (Project)


type Dialog
    = AddProjectDialog Dialog.AddProject.Model
    | EditProjectDialog Dialog.EditProject.Model
    | AddLabelDialog
    | EditLabelDialog LabelId
    | AddFilterDialog
    | EditFilterDialog FilterId
    | NoDialog


type alias Config msg =
    { addProject : Dialog.AddProject.Config msg
    , editProject : Dialog.EditProject.Config msg
    }


type DialogMsg
    = AddProjectDialogMsg Dialog.AddProject.Msg
    | EditProjectDialogMsg Dialog.EditProject.Msg


initAddProjectDialogAt : Config msg -> Int -> ( Dialog, Cmd msg )
initAddProjectDialogAt config idx =
    Dialog.AddProject.initAt config.addProject idx
        |> Tuple.mapFirst AddProjectDialog


initEditProjectDialog : Config msg -> Project -> ( Dialog, Cmd msg )
initEditProjectDialog config project =
    Dialog.EditProject.init config.editProject project
        |> Tuple.mapFirst EditProjectDialog


viewDialog : Config msg -> Dialog -> List (Html msg)
viewDialog config dialog =
    case dialog of
        AddProjectDialog model ->
            [ Dialog.AddProject.view config.addProject model ]

        EditProjectDialog model ->
            [ Dialog.EditProject.view config.editProject model ]

        _ ->
            []


dialogSubscriptions : Config msg -> Dialog -> Sub msg
dialogSubscriptions config dialog =
    case dialog of
        AddProjectDialog model ->
            Dialog.AddProject.subscriptions config.addProject model

        EditProjectDialog model ->
            Dialog.EditProject.subscriptions config.editProject model

        _ ->
            Sub.none
