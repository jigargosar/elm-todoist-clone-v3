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
import Html.Styled as H exposing (Attribute, Html, div, text)
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
    | DNDListMsg (DNDList.Msg Project)


subscriptions : ProjectPanel -> Sub Msg
subscriptions projectPanel =
    case projectPanel of
        Collapsed ->
            Sub.none

        Expanded dnd ->
            DNDList.subscriptions DNDListMsg dnd


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

        DNDListMsg msg ->
            case model of
                Expanded dnd ->
                    DNDList.update (config.toMsg << DNDListMsg)
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
            case DNDList.view DNDListMsg projectList dndList of
                DNDList.WhenNotDragging { dragHandleAttrs, items } ->
                    [ viewExpanded
                    , List.map (viewItem dragHandleAttrs) items
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


viewCollapsed : List (Html Msg)
viewCollapsed =
    []


viewExpanded : List (Html Msg)
viewExpanded =
    []


viewItem : (Project -> String -> List (Attribute msg)) -> Project -> Html msg
viewItem dragHandleAttrs project =
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
                :: dragHandleAttrs project domId
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


viewDNDGhost : (Msg -> msg) -> ProjectPanel -> Maybe (List (Html msg))
viewDNDGhost toMsg =
    getDND
        >> Maybe.andThen DNDList.ghost
        >> Maybe.map
            (\( styles, project ) ->
                [ div
                    [ css [ lh 1.5, flex, batch styles ] ]
                    [ div
                        (css [ Px.p2 8 8, pointer ] :: [])
                        [ text "DRAG_HANDLE" ]
                    , div [ css [ Px.p2 8 8 ] ] [ text <| Project.title project ]
                    ]
                ]
                    |> List.map (H.map toMsg)
            )
