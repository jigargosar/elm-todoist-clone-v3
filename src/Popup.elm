module Popup exposing (..)

import FilterId exposing (FilterId)
import LabelId exposing (LabelId)
import PopupView
import ProjectId exposing (ProjectId)


type Popup
    = ProjectMoreMenu ProjectId
    | LabelMoreMenu LabelId
    | FilterMoreMenu FilterId


type PopupMsg
    = ProjectMoreMenuMsg PopupView.ProjectMenuItem
    | LabelMoreMenuMsg PopupView.LabelMenuItem
    | FilterMoreMenuMsg PopupView.FilterMenuItem
