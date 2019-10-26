module ProjectPanel exposing
    ( Msg
    , ProjectPanel
    , ProjectPanelConfig
    , initial
    , subscriptions
    , update
    , view
    , viewDNDGhost
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
    = Collapsed
    | Expanded (DNDList.Model Project)


initial : ProjectPanel
initial =
    Expanded DNDList.init



-- PROJECT PANEL UPDATE


type Msg
    = HeaderClicked
    | AddClicked
    | DNDList (DNDList.Msg Project)


subscriptions : ProjectPanel -> Sub Msg
subscriptions projectPanel =
    case projectPanel of
        Collapsed ->
            Sub.none

        Expanded dnd ->
            DNDList.subscriptions DNDList dnd


type alias ProjectPanelConfig msg =
    { toMsg : Msg -> msg
    , projectOrderChanged : List Project -> msg
    }


update : ProjectPanelConfig msg -> Msg -> ProjectPanel -> ( ProjectPanel, Cmd msg )
update config message model =
    case message of
        HeaderClicked ->
            ( model, Cmd.none )

        AddClicked ->
            ( model, Cmd.none )

        DNDList msg ->
            case model of
                Expanded dnd ->
                    DNDList.update (config.toMsg << DNDList)
                        { onComplete = config.projectOrderChanged }
                        msg
                        dnd
                        |> Tuple.mapFirst Expanded

                _ ->
                    ( model, Cmd.none )



-- PROJECT PANEL VIEW


view : List Project -> ProjectPanel -> List (Html Msg)
view projectList model =
    case model of
        Collapsed ->
            viewCollapsed

        Expanded dndList ->
            case DNDList.view DNDList projectList dndList of
                DNDList.WhenNotDragging config ->
                    [ viewExpanded
                    , List.map (viewItem config) config.items
                    ]
                        |> List.concat

                DNDList.WhenDragging config ->
                    [ viewExpanded
                    , List.map (viewItemWhenDragActive config) config.items
                    ]
                        |> List.concat


getDND : ProjectPanel -> Maybe (DNDList.Model Project)
getDND model =
    case model of
        Expanded dnd ->
            Just dnd

        Collapsed ->
            Nothing


viewDNDGhost : ProjectPanel -> List (Html Msg)
viewDNDGhost =
    getDND
        >> Maybe.andThen DNDList.ghost
        >> Maybe.map (\( styles, project ) -> [ div [] [ text "ghost" ] ])
        >> Maybe.withDefault []


viewCollapsed : List (Html Msg)
viewCollapsed =
    []


viewExpanded : List (Html Msg)
viewExpanded =
    []


viewItem : DNDList.NotDraggingConfig Project msg -> Project -> Html msg
viewItem config project =
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


viewItemWhenDragActive : DNDList.DraggingConfig Project msg -> Project -> Html msg
viewItemWhenDragActive config project =
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
