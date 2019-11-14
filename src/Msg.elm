module Msg exposing (..)

import Browser exposing (UrlRequest)
import Dialog
import Dialog.AddProject as AddProject
import Dialog.EditProject as EditProject
import Filter exposing (Filter)
import FilterPanel
import Label exposing (Label)
import LabelPanel
import Popper
import Popup
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import ProjectPanel
import Timestamp exposing (Timestamp)
import TodoId exposing (TodoId)
import Url exposing (Url)


type SubMsg
    = ProjectPanel ProjectPanel.Msg
    | LabelPanel LabelPanel.Msg
    | FilterPanel FilterPanel.Msg
    | Dialog Dialog.Msg
    | Popper Popper.Msg


type Msg
    = NoOp
    | LogError String
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | ToggleTodoCompleted TodoId
    | OpenDrawerModal
    | CloseDrawerModal
    | PopupTriggered Popup.Popup String
    | ClosePopup
    | PopupMsg Popup.PopupMsg
    | AddProjectDialogSaved AddProject.SavedWith
    | AddProjectWithTS AddProject.SavedWith Timestamp
    | EditProjectDialogSaved EditProject.SavedWith
    | EditProjectWithTS EditProject.SavedWith Timestamp
    | AddProjectClicked
    | EditProjectClicked ProjectId
    | AddLabelClicked
    | AddFilterClicked
    | SubMsg SubMsg
    | ProjectOrderChanged (List Project)
    | LabelOrderChanged (List Label)
    | FilterOrderChanged (List Filter)
