module Dialog exposing (..)

import FilterId exposing (FilterId)
import LabelId exposing (LabelId)
import ProjectId exposing (ProjectId)


type Dialog
    = AddProject
    | EditProject ProjectId
    | AddLabel
    | EditLabel LabelId
    | AddFilter
    | EditFilter FilterId
