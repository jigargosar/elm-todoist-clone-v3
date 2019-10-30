module Dialog exposing (Dialog(..), initAddProjectDialog, view)

import Dialog.AddProject
import FilterId exposing (FilterId)
import Html.Styled exposing (Attribute, Html)
import LabelId exposing (LabelId)
import ProjectId exposing (ProjectId)


initAddProjectDialog : Dialog
initAddProjectDialog =
    AddProjectDialog <| Dialog.AddProject.initial


type Dialog
    = AddProjectDialog Dialog.AddProject.Model
    | EditProjectDialog ProjectId
    | AddLabelDialog
    | EditLabelDialog LabelId
    | AddFilterDialog
    | EditFilterDialog FilterId
    | NoDialog


type alias Config msg =
    { cancel : msg }


view : Config msg -> Dialog -> List (Html msg)
view config dialog =
    case dialog of
        AddProjectDialog model ->
            Dialog.AddProject.view config model

        _ ->
            []
