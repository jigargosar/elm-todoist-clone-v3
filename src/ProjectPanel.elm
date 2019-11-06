module ProjectPanel exposing
    ( Config
    , Msg
    , ProjectPanel
    , createConfig
    , initial
    , subscriptions
    , update
    , view
    , viewGhost
    )

import Css
import DNDList as DND exposing (DNDList)
import DrawerUI
import ExpansionPanel as EP exposing (Collapsible)
import Html.Styled exposing (Attribute, Html, text)
import Html.Styled.Attributes as A exposing (href)
import Html.Styled.Events exposing (onClick)
import PanelsHelp
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Route
import Styles exposing (..)
import UI.Icon as Icon exposing (Icon)


type alias ProjectPanel =
    { collapsible : Collapsible
    , dnd : DNDList Project
    }


initial : ProjectPanel
initial =
    { collapsible = EP.expanded
    , dnd = DND.initial
    }



-- PROJECT PANEL UPDATE


subscriptions : Config msg -> ProjectPanel -> Sub msg
subscriptions config { dnd } =
    Sub.batch
        [ DND.subscriptions config.dnd dnd
        ]


type alias Config msg =
    { moreClicked : ProjectId -> String -> msg
    , dnd : DND.Config Project msg
    , ep : EP.Config msg
    , icon : Icon
    }


createConfig :
    { toMsg : Msg -> msg
    , addClicked : msg
    , moreClicked : ProjectId -> String -> msg
    , sorted : List Project -> msg
    }
    -> Config msg
createConfig { toMsg, addClicked, moreClicked, sorted } =
    let
        ep =
            { toggled = toMsg Toggled
            , title = "Projects"
            , secondary = { iconName = "add", action = addClicked }
            }
    in
    { moreClicked = moreClicked
    , dnd = { toMsg = toMsg << DNDList, sorted = sorted }
    , ep = ep
    , icon = Icon.Folder
    }


type Msg
    = DNDList (DND.Msg Project)
    | Toggled


update : Config msg -> Msg -> ProjectPanel -> ( ProjectPanel, Cmd msg )
update config message model =
    case message of
        DNDList msg ->
            DND.update config.dnd msg model.dnd
                |> Tuple.mapFirst (\dnd -> { model | dnd = dnd })

        Toggled ->
            ( { model | collapsible = EP.toggle model.collapsible }, Cmd.none )


viewGhost : Config msg -> ProjectPanel -> List (Html msg)
viewGhost config { dnd } =
    case DND.ghost dnd of
        Just ( style, project ) ->
            [ let
                { iconColor, title, href } =
                    projectData project
              in
              DrawerUI.item [ style ]
                []
                [ DrawerUI.dragHandle [ c_ iconColor ] [] PanelsHelp.projectIcon
                , DrawerUI.link [] [ href ] [ text title ]
                , DrawerUI.more []
                ]
            ]

        Nothing ->
            []


view : Config msg -> List Project -> ProjectPanel -> List (Html msg)
view config projectList state =
    EP.viewHeader config.ep state.collapsible
        :: EP.viewContent
            (\_ ->
                viewItems config
                    projectList
                    state.dnd
            )
            state.collapsible


viewItems :
    Config msg
    -> List Project
    -> DNDList Project
    -> List (Html msg)
viewItems config projectList dnd =
    let
        { dragStartAttrs, dragOverAttrs, isBeingDragged, items } =
            DND.view config.dnd projectList dnd
    in
    List.map
        (viewItemHelp config
            { dragStartAttrs = dragStartAttrs
            , dragOverAttrs = dragOverAttrs
            , isBeingDragged = isBeingDragged
            }
        )
        items


viewItemHelp config { dragStartAttrs, dragOverAttrs, isBeingDragged } project =
    let
        domId =
            itemDomId project

        moreDomId =
            domId ++ "__more-btn"
    in
    viewItem
        { itemAttrs = A.id domId :: dragOverAttrs project
        , itemStyles = [ styleIf (isBeingDragged project) [ Css.opacity <| Css.zero ] ]
        , handleAttrs = dragStartAttrs project domId
        , moreAttrs =
            [ A.id moreDomId
            , onClick (config.moreClicked (Project.id project) moreDomId)
            ]
        }
        (projectData project)


itemDomId : Project -> String
itemDomId project =
    "project-panel-item__" ++ (Project.id project |> ProjectId.toString)


type alias ItemData msg =
    { title : String
    , iconColor : Css.Color
    , href : Attribute msg
    }


projectData : Project -> ItemData msg
projectData project =
    let
        title =
            Project.title project

        iconColor =
            Project.cssColor project

        href =
            Route.projectHref project
    in
    ItemData title iconColor href


viewItem : ItemProps msg -> ItemData msg -> Html msg
viewItem { itemAttrs, itemStyles, handleAttrs, moreAttrs } { title, iconColor, href } =
    DrawerUI.item itemStyles
        itemAttrs
        [ DrawerUI.dragHandle [ c_ iconColor ] handleAttrs PanelsHelp.projectIcon
        , DrawerUI.link [] [ href ] [ text title ]
        , DrawerUI.more moreAttrs
        ]


type alias ItemProps msg =
    { itemAttrs : List (Attribute msg)
    , itemStyles : List Style
    , handleAttrs : List (Attribute msg)
    , moreAttrs : List (Attribute msg)
    }
