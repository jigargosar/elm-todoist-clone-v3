module ProjectPanel exposing
    ( ProjectPanel
    , ProjectPanelConfig
    , ProjectPanelMsg
    , initialProjectPanel
    , projectPanelSubscriptions
    , updateProjectPanel
    , viewProjectPanel
    )

import Css
import DNDList
import Html.Styled exposing (Attribute, Html, div, text)
import Html.Styled.Attributes as A exposing (css)
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Px
import Styles exposing (..)


type ProjectPanel
    = ProjectPanelCollapsed
    | ProjectPanelExpanded (DNDList.Model Project)


initialProjectPanel : ProjectPanel
initialProjectPanel =
    ProjectPanelExpanded DNDList.init



-- PROJECT PANEL UPDATE


type ProjectPanelMsg
    = ProjectPanelHeaderClicked
    | ProjectPanelAddClicked
    | ProjectPanelDND (DNDList.Msg Project)


projectPanelSubscriptions : ProjectPanel -> Sub ProjectPanelMsg
projectPanelSubscriptions projectPanel =
    case projectPanel of
        ProjectPanelCollapsed ->
            Sub.none

        ProjectPanelExpanded dnd ->
            DNDList.subscriptions ProjectPanelDND dnd


type alias ProjectPanelConfig msg =
    { toMsg : ProjectPanelMsg -> msg
    , projectOrderChanged : List Project -> msg
    }


updateProjectPanel : ProjectPanelConfig msg -> ProjectPanelMsg -> ProjectPanel -> ( ProjectPanel, Cmd msg )
updateProjectPanel config message model =
    case message of
        ProjectPanelHeaderClicked ->
            ( model, Cmd.none )

        ProjectPanelAddClicked ->
            ( model, Cmd.none )

        ProjectPanelDND msg ->
            case model of
                ProjectPanelExpanded dnd ->
                    DNDList.update (config.toMsg << ProjectPanelDND)
                        { onComplete = config.projectOrderChanged }
                        msg
                        dnd
                        |> Tuple.mapFirst ProjectPanelExpanded

                _ ->
                    ( model, Cmd.none )



-- PROJECT PANEL VIEW


viewProjectPanel : List Project -> ProjectPanel -> List (Html ProjectPanelMsg)
viewProjectPanel projectList model =
    case model of
        ProjectPanelCollapsed ->
            viewProjectPanelHeaderCollapsed

        ProjectPanelExpanded dnd ->
            case DNDList.view ProjectPanelDND projectList dnd of
                DNDList.WhenNotDragging config ->
                    [ viewProjectPanelHeaderExpanded
                    , List.map (viewProjectPanelItem config) config.items
                    ]
                        |> List.concat

                DNDList.WhenDragging config ->
                    [ viewProjectPanelHeaderExpanded
                    , List.map (viewProjectPanelItemWhenDragActive config) config.items
                    ]
                        |> List.concat


viewProjectPanelHeaderCollapsed : List (Html ProjectPanelMsg)
viewProjectPanelHeaderCollapsed =
    []


viewProjectPanelHeaderExpanded : List (Html ProjectPanelMsg)
viewProjectPanelHeaderExpanded =
    []


viewProjectPanelItem : DNDList.NotDraggingConfig Project msg -> Project -> Html msg
viewProjectPanelItem config project =
    let
        domId =
            "project-panel-item__" ++ (Project.id project |> ProjectId.toString)
    in
    div
        [ A.id domId
        , css [ lh 1.5, flex ]
        ]
        [ div
            (css [ Px.p2 8 8, pointer ]
                :: config.dragHandleAttrs project domId
            )
            [ text "DRAG_HANDLE" ]
        , div [ css [ Px.p2 8 8 ] ] [ text <| Project.title project ]
        ]


viewProjectPanelItemWhenDragActive : DNDList.DraggingConfig Project msg -> Project -> Html msg
viewProjectPanelItemWhenDragActive config project =
    let
        isBeingDragged =
            config.isBeingDragged project

        dragOverAttributes =
            config.dragOverAttrs project

        dragOverStyle =
            styleIf isBeingDragged [ Css.opacity <| Css.zero ]
    in
    div
        (css [ lh 1.5, flex, dragOverStyle ] :: dragOverAttributes)
        [ div
            (css [ Px.p2 8 8, pointer ] :: [])
            [ text "DRAG_HANDLE" ]
        , div [ css [ Px.p2 8 8 ] ] [ text <| Project.title project ]
        ]
