module Dialog exposing
    ( Config
    , Dialog
    , Msg
    , System
    , createConfig
    , subscriptions
    , system
    , update
    , view
    )

-- DIALOG

import Browser.Dom as Dom
import CColor exposing (CColor)
import Dialog.AddProject as AddProject
import Dialog.EditProject as EditProject
import Dialog.SelectColor as SelectColor
import Focus exposing (FocusResult)
import Html.Styled as H exposing (Html)
import Log
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Ret exposing (Ret, RetF)


type alias System msg =
    { initial : Dialog
    , subscriptions : Dialog -> Sub msg
    , openAddProject : Int -> msg
    , openEditProject : Project -> msg
    , update : Msg -> Dialog -> Ret Dialog msg
    , view : Dialog -> Html msg
    }


system :
    { toMsg : Msg -> msg
    , projectAdded : AddProject.SavedWith -> msg
    , projectEdited : EditProject.SavedWith -> msg
    }
    -> System msg
system ({ toMsg } as configParams) =
    let
        config : Config msg
        config =
            createConfig configParams
    in
    { initial = Closed
    , openAddProject = toMsg << OpenAddProject
    , openEditProject = toMsg << OpenEditProject
    , subscriptions = subscriptions config
    , update = update config
    , view = view config
    }


type alias Config msg =
    { toMsg : Msg -> msg
    , focusCmd : String -> Cmd msg
    , projectAddedCmd : AddProject.SavedWith -> Cmd msg
    , projectEditedCmd : EditProject.SavedWith -> Cmd msg
    , apSys : AddProject.System msg
    , epSys : EditProject.System msg
    }


createConfig :
    { toMsg : Msg -> msg
    , projectAdded : AddProject.SavedWith -> msg
    , projectEdited : EditProject.SavedWith -> msg
    }
    -> Config msg
createConfig { toMsg, projectAdded, projectEdited } =
    { toMsg = toMsg
    , focusCmd = Focus.cmd (toMsg << Focused)
    , projectAddedCmd = Ret.toCmd << projectAdded
    , projectEditedCmd = Ret.toCmd << projectEdited
    , apSys =
        AddProject.system
            { toMsg = toMsg << AddProjectMsg
            , canceled = toMsg Canceled
            , saved = toMsg << AddProjectSaved
            }
    , epSys =
        EditProject.system
            { toMsg = toMsg << EditProjectMsg
            , canceled = toMsg Canceled
            , saved = toMsg << EditProjectSaved
            }
    }


type Dialog
    = AddProject APModel
    | EditProject EPModel
      {- | AddLabelDialog
         | EditLabelDialog LabelId
         | AddFilterDialog
         | EditFilterDialog FilterId
      -}
    | Closed


type alias APModel =
    { title : String
    , favorite : Bool
    , selectColor : SelectColor.Model
    , cColor : CColor
    , idx : Int
    }


type alias EPModel =
    { projectId : ProjectId
    , title : String
    , favorite : Bool
    , selectColor : SelectColor.Model
    , cColor : CColor
    }


type Msg
    = AddProjectMsg AddProject.Msg
    | EditProjectMsg EditProject.Msg
    | OpenAddProject Int
    | OpenEditProject Project
    | AddProjectSaved AddProject.SavedWith
    | EditProjectSaved EditProject.SavedWith
    | Canceled
    | Focused FocusResult


type APMsg
    = AP_Title String
    | AP_SelectColor SelectColor.Msg
    | AP_CColor CColor
    | AP_Favorite Bool
    | AP_Save


type EPMsg
    = EP_Submit
    | EP_Title String
    | EP_SelectColor SelectColor.Msg
    | EP_CColor CColor
    | EP_Favorite Bool


subscriptions : Config msg -> Dialog -> Sub msg
subscriptions c dialog =
    case dialog of
        AddProject model ->
            c.apSys.subscriptions model

        EditProject model ->
            c.epSys.subscriptions model

        Closed ->
            Sub.none


update : Config msg -> Msg -> Dialog -> Ret Dialog msg
update c message model =
    case message of
        AddProjectMsg msg ->
            case model of
                AddProject sub ->
                    c.apSys.update msg sub |> Ret.map AddProject

                _ ->
                    Ret.only model

        EditProjectMsg msg ->
            case model of
                EditProject sub ->
                    c.epSys.update msg sub |> Ret.map EditProject

                _ ->
                    Ret.only model

        OpenAddProject idx ->
            c.apSys.initAt idx |> Tuple.mapBoth AddProject c.focusCmd

        OpenEditProject project ->
            c.epSys.init project |> Tuple.mapBoth EditProject c.focusCmd

        Focused result ->
            case result of
                Err (Dom.NotFound domId) ->
                    ( model, Log.logError <| "dialog focus failed: " ++ domId )

                Ok () ->
                    ( model, Cmd.none )

        Canceled ->
            Ret.only Closed

        AddProjectSaved savedWith ->
            ( Closed, c.projectAddedCmd savedWith )

        EditProjectSaved savedWith ->
            ( Closed, c.projectEditedCmd savedWith )


view : Config msg -> Dialog -> Html msg
view c dialog =
    case dialog of
        AddProject model ->
            c.apSys.view model

        EditProject model ->
            c.epSys.view model

        Closed ->
            H.text ""
