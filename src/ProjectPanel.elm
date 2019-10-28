module ProjectPanel exposing
    ( Config
    , ProjectPanel
    , initial
    , onDNDMsg
    , onToggle
    , subscriptions
    , view
    )

import Css
import DNDList
import Html.Styled exposing (Attribute, Html, a, button, div, i, text)
import Html.Styled.Attributes as A exposing (class, css, href)
import Html.Styled.Events exposing (onClick)
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Px
import Route
import Styles exposing (..)
import UI


type ProjectPanel
    = Collapsed
    | Expanded (DNDList.Model Project)


initial : ProjectPanel
initial =
    Expanded DNDList.init



-- PROJECT PANEL UPDATE


subscriptions : Config msg -> ProjectPanel -> Sub msg
subscriptions config projectPanel =
    case projectPanel of
        Collapsed ->
            Sub.none

        Expanded dnd ->
            DNDList.subscriptions config.dndListMsg dnd


type alias Config msg =
    { toggled : msg
    , dndListMsg : DNDList.Msg Project -> msg
    , sorted : List Project -> msg
    , addClicked : msg
    , moreClicked : ProjectId -> String -> msg
    }


onToggle : ProjectPanel -> ProjectPanel
onToggle model =
    case model of
        Collapsed ->
            Expanded DNDList.init

        Expanded _ ->
            Collapsed


onDNDMsg :
    Config msg
    -> DNDList.Msg Project
    -> ProjectPanel
    -> ( ProjectPanel, Cmd msg )
onDNDMsg config msg model =
    case model of
        Expanded dnd ->
            DNDList.update config.dndListMsg
                { onComplete = config.sorted }
                msg
                dnd
                |> Tuple.mapFirst Expanded

        Collapsed ->
            ( model, Cmd.none )



-- PROJECT PANEL VIEW


view : Config msg -> List Project -> ProjectPanel -> ( List (Html msg), List (Html msg) )
view config projectList model =
    let
        viewHeader isExpanded =
            UI.viewExpansionPanelHeader
                { toggled = config.toggled
                , title = "Projects"
                , isExpanded = isExpanded
                , secondary = { iconName = "add", action = config.addClicked }
                }
    in
    case model of
        Collapsed ->
            ( [ viewHeader False ], [] )

        Expanded dnd ->
            let
                ( items, ghost ) =
                    viewItems config projectList dnd
            in
            ( viewHeader True :: items, ghost )


viewItems :
    { a
        | dndListMsg : DNDList.Msg Project -> msg
        , moreClicked : ProjectId -> String -> msg
    }
    -> List Project
    -> DNDList.Model Project
    -> ( List (Html msg), List (Html msg) )
viewItems config projectList dndList =
    case DNDList.view config.dndListMsg projectList dndList of
        DNDList.WhenNotDragging { dragHandleAttrs, items } ->
            ( List.map
                (\project ->
                    let
                        domId =
                            itemDomId project

                        moreDomId =
                            domId ++ "__more-btn"
                    in
                    viewItem
                        { itemAttrs = [ A.id domId ]
                        , itemStyles = []
                        , handleAttrs = dragHandleAttrs project domId
                        , moreAttrs =
                            [ A.id moreDomId
                            , onClick (config.moreClicked (Project.id project) moreDomId)
                            ]
                        }
                        project
                )
                items
            , []
            )

        DNDList.WhenDragging { isBeingDragged, dragOverAttrs, items, ghost } ->
            ( List.map
                (\project ->
                    viewItem
                        { itemAttrs = dragOverAttrs project
                        , itemStyles = [ styleIf (isBeingDragged project) [ Css.opacity <| Css.zero ] ]
                        , handleAttrs = []
                        , moreAttrs = []
                        }
                        project
                )
                items
            , let
                ( ghostStyles, ghostProject ) =
                    ghost
              in
              [ viewItem
                    { itemAttrs = []
                    , itemStyles = [ ghostStyles ]
                    , handleAttrs = []
                    , moreAttrs = []
                    }
                    ghostProject
              ]
            )


itemDomId : Project -> String
itemDomId project =
    "project-panel-item__" ++ (Project.id project |> ProjectId.toString)


viewItem : ItemProps msg -> Project -> Html msg
viewItem { itemAttrs, itemStyles, handleAttrs, moreAttrs } project =
    let
        title =
            Project.title project

        iconColor =
            Project.cssColor project

        projectId =
            Project.id project

        href =
            Route.href (Route.Project projectId)
    in
    div (css [ Px.pl 4, Px.pr (4 + 16), flex, batch itemStyles ] :: class "hover_parent" :: itemAttrs)
        [ i
            (css [ Px.pa 4, Px.m2 4 0, cursorMove, c_ iconColor ]
                :: class "material-icons"
                :: handleAttrs
            )
            [ text "folder" ]
        , a [ css [ linkReset, Px.p2 8 4, lh 1.5, flexGrow1 ], href ] [ text title ]
        , button
            ([ css [ btnReset, pointer, Px.pa 4, Px.m2 4 0, flex, itemsCenter, selfEnd ]
             , class "show_on_parent_hover"
             ]
                ++ moreAttrs
            )
            [ i [ class "material-icons" ] [ text "more_horiz" ]
            ]
        ]


type alias ItemProps msg =
    { itemAttrs : List (Attribute msg)
    , itemStyles : List Style
    , handleAttrs : List (Attribute msg)
    , moreAttrs : List (Attribute msg)
    }
