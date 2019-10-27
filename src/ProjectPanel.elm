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
import Css.Global
import DNDList
import Html.Styled as H exposing (Attribute, Html, button, div, i, span, text)
import Html.Styled.Attributes as A exposing (class, css)
import Html.Styled.Events exposing (onClick)
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Px
import Styles exposing (..)
import Theme


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
            ( case model of
                Collapsed ->
                    Expanded DNDList.init

                Expanded _ ->
                    Collapsed
            , Cmd.none
            )

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
            [ viewExpanded
            , case DNDList.view DNDListMsg projectList dndList of
                DNDList.WhenNotDragging { dragHandleAttrs, items } ->
                    List.map (viewItemWhenNotDragging dragHandleAttrs) items

                DNDList.WhenDragging { isBeingDragged, dragOverAttrs, items } ->
                    List.map (viewItemWhenDragging isBeingDragged dragOverAttrs) items
            ]
                |> List.concat


itemDomId : Project -> String
itemDomId project =
    "project-panel-item__" ++ (Project.id project |> ProjectId.toString)


getDND : ProjectPanel -> Maybe (DNDList.Model Project)
getDND model =
    case model of
        Expanded dnd ->
            Just dnd

        Collapsed ->
            Nothing


getDNDGhost : ProjectPanel -> Maybe ( Style, Project )
getDNDGhost =
    getDND >> Maybe.andThen DNDList.ghost


viewCollapsed : List (Html Msg)
viewCollapsed =
    viewHeader False


viewExpanded : List (Html Msg)
viewExpanded =
    viewHeader True


viewHeader : Bool -> List (Html Msg)
viewHeader isExpanded =
    let
        iconName =
            if isExpanded then
                "expand_more"

            else
                "chevron_right"
    in
    [ div
        [ css [ Px.pl 4, Px.pr (4 + 16), flex, itemsCenter, bo_b, boc Theme.borderGray, hover [ bgGrayL 0.95 ] ] ]
        [ button
            [ css [ btnReset, pointer, flexGrow1, flex, itemsCenter, tal ], onClick HeaderClicked ]
            [ i [ css [ Px.pa 4 ], class "material-icons" ] [ text iconName ]
            , span [ css [ Px.p2 8 4, bold, flexGrow1 ] ] [ text "Projects" ]
            ]
        , button
            [ css [ btnReset, pointer, Px.pa 4, Px.m2 4 0, flex, itemsCenter, selfEnd ]
            ]
            [ i
                [ class "material-icons" ]
                [ text "add" ]
            ]
        ]
    ]


viewItem : ItemProps msg -> Project -> Html msg
viewItem { itemAttrs, itemStyles, handleAttrs } project =
    let
        title =
            Project.title project

        iconColor =
            Project.cssColor project
    in
    div (css [ Px.p2 0 4, flex, batch itemStyles ] :: class "hover_parent" :: itemAttrs)
        [ i
            (css [ Px.pa 4, Px.m2 4 0, cursorMove, c_ iconColor ]
                :: class "material-icons"
                :: handleAttrs
            )
            [ text "folder" ]
        , div [ css [ Px.p2 8 4, lh 1.5, flexGrow1 ] ] [ text title ]
        , button
            ([ css [ btnReset, pointer, Px.pa 4, Px.m2 4 0, flex, itemsCenter, selfEnd ]
             , class "show_on_parent_hover"
             ]
                ++ handleAttrs
            )
            [ i [ class "material-icons" ] [ text "more_horiz" ]
            ]
        ]


viewItemWhenNotDragging : (Project -> String -> List (Attribute msg)) -> Project -> Html msg
viewItemWhenNotDragging dragHandleAttrs project =
    let
        domId =
            itemDomId project
    in
    viewItem
        { itemAttrs = [ A.id domId ]
        , itemStyles = []
        , handleAttrs = dragHandleAttrs project domId
        }
        project


viewItemWhenDragging :
    (Project -> Bool)
    -> (Project -> List (Attribute msg))
    -> Project
    -> Html msg
viewItemWhenDragging isBeingDragged dragOverAttrs project =
    viewItem
        { itemAttrs = dragOverAttrs project
        , itemStyles = [ styleIf (isBeingDragged project) [ Css.opacity <| Css.zero ] ]
        , handleAttrs = []
        }
        project


viewGhostItem itemStyle project =
    [ viewItem { itemAttrs = [], itemStyles = [ itemStyle ], handleAttrs = [] } project ]


viewDNDGhost : (Msg -> msg) -> ProjectPanel -> List (Html msg)
viewDNDGhost toMsg =
    getDNDGhost
        >> Maybe.map
            (\( itemStyle, project ) ->
                viewGhostItem itemStyle project
                    |> List.map (H.map toMsg)
            )
        >> Maybe.withDefault []


type alias ItemProps msg =
    { itemAttrs : List (Attribute msg)
    , itemStyles : List Style
    , handleAttrs : List (Attribute msg)
    }
