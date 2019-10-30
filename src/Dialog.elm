module Dialog exposing (Dialog(..), initAddProject, view)

import Dialog.AddProject
import FilterId exposing (FilterId)
import Html.Styled exposing (Attribute, Html)
import LabelId exposing (LabelId)
import ProjectId exposing (ProjectId)


initAddProject : Dialog
initAddProject =
    AddProject <| Dialog.AddProject.initial


type Dialog
    = AddProject Dialog.AddProject.Model
    | EditProject ProjectId
    | AddLabel
    | EditLabel LabelId
    | AddFilter
    | EditFilter FilterId
    | None


type alias Config msg =
    { cancel : msg }


view : Config msg -> Dialog -> List (Html msg)
view config dialog =
    case dialog of
        AddProject model ->
            Dialog.AddProject.view config model

        _ ->
            []
