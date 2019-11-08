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

import Dialog.AddProject as AddProject exposing (AddProject)
import Dialog.EditProject as EditProject exposing (EditProject)
import Html.Styled exposing (Html)
import Optional
import Project exposing (Project)
import Ret exposing (Ret)


type Dialog
    = AddProject AddProject
    | EditProject EditProject
      {- | AddLabelDialog
         | EditLabelDialog LabelId
         | AddFilterDialog
         | EditFilterDialog FilterId
      -}
    | Closed


type alias Config msg =
    { addProject : AddProject.System msg
    , editProject : EditProject.System msg
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
        sys : (a -> SubMsg) -> (b -> SavedMsg) -> { toMsg : a -> msg, canceled : msg, saved : b -> msg }
        sys subM saveM =
            { toMsg = c.toMsg << SubMsg << subM
            , canceled = c.toMsg Canceled
            , saved = c.toMsg << SavedMsg << saveM
            }
    in
    { addProject = AddProject.system <| sys AddProjectMsg AddProjectSaved
    , editProject = EditProject.system <| sys EditProjectMsg EditProjectSaved
    , projectAdded = c.projectAdded
    , projectEdited = c.projectEdited
    }


type SavedMsg
    = AddProjectSaved AddProject.SavedWith
    | EditProjectSaved EditProject.SavedWith


type SubMsg
    = AddProjectMsg AddProject.Msg
    | EditProjectMsg EditProject.Msg


type Msg
    = SubMsg SubMsg
    | OpenAddProject Int
    | OpenEditProject Project
    | SavedMsg SavedMsg
    | Canceled


openAddProject : Int -> Msg
openAddProject =
    OpenAddProject


openEditProject : Project -> Msg
openEditProject =
    OpenEditProject


initial =
    Closed


subscriptions : Config msg -> Dialog -> Sub msg
subscriptions config dialog =
    case dialog of
        AddProject model ->
            config.addProject.subscriptions model

        EditProject model ->
            config.editProject.subscriptions model

        _ ->
            Sub.none


updateF : Config msg -> Msg -> Ret Dialog msg -> Ret Dialog msg
updateF config message =
    case message of
        SubMsg subMsg ->
            updateSub config subMsg

        OpenAddProject idx ->
            Ret.andThenAlways (config.addProject.initAt idx)
                >> Ret.map AddProject

        OpenEditProject project ->
            Ret.andThenAlways (config.editProject.init project)
                >> Ret.map EditProject

        Canceled ->
            Ret.always Closed

        SavedMsg savedMsg ->
            Ret.always Closed
                >> Ret.addMsg
                    (case savedMsg of
                        AddProjectSaved savedWith ->
                            config.projectAdded savedWith

                        EditProjectSaved savedWith ->
                            config.projectEdited savedWith
                    )


update : Config msg -> Msg -> Dialog -> ( Dialog, Cmd msg )
update config =
    Ret.fromUpdateF (updateF config)


updateSub : Config msg -> SubMsg -> Ret Dialog msg -> Ret Dialog msg
updateSub config subMsg =
    case subMsg of
        AddProjectMsg msg ->
            Ret.updateOptionalF fields.addProject
                config.addProject.updateF
                msg

        EditProjectMsg msg ->
            Ret.andThen
                (Ret.updateOptional fields.editProject
                    config.editProject.update
                    msg
                )


view : Config msg -> Dialog -> List (Html msg)
view config dialog =
    case dialog of
        AddProject model ->
            [ config.addProject.view model ]

        EditProject model ->
            [ config.editProject.view model ]

        Closed ->
            []


fields =
    { addProject =
        Optional.fromTuple
            ( \b ->
                case b of
                    AddProject s ->
                        Just s

                    _ ->
                        Nothing
            , \s _ -> AddProject s
            )
    , editProject =
        Optional.fromTuple
            ( \b ->
                case b of
                    EditProject s ->
                        Just s

                    _ ->
                        Nothing
            , \s _ -> EditProject s
            )
    }
