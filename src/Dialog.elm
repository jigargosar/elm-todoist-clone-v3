module Dialog exposing (Dialog(..))

import Dialog.AddProject
import FilterId exposing (FilterId)
import LabelId exposing (LabelId)
import ProjectId exposing (ProjectId)


type Dialog
    = AddProjectDialog Dialog.AddProject.Model
    | EditProjectDialog ProjectId
    | AddLabelDialog
    | EditLabelDialog LabelId
    | AddFilterDialog
    | EditFilterDialog FilterId
    | NoDialog
