module ProjectPanel exposing
    ( Msg
    , ProjectPanel
    , System
    , system
    )

import Css
import DNDList as DND exposing (DNDList)
import DrawerUI
import ExpansionPanel as EP exposing (Collapsible)
import Html.Styled exposing (Attribute, Html, text)
import Html.Styled.Attributes as A exposing (href)
import Html.Styled.Events exposing (onClick)
import Lens exposing (Lens)
import PanelsHelp
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Ret exposing (Ret)
import Route
import Styles exposing (..)



-- SYSTEM


type alias System msg =
    { initial : ProjectPanel
    , subscriptions : ProjectPanel -> Sub msg
    , update : Msg -> Ret ProjectPanel msg -> Ret ProjectPanel msg
    , view : List Project -> ProjectPanel -> List (Html msg)
    , viewGhost : ProjectPanel -> List (Html msg)
    }


system :
    { toMsg : Msg -> msg
    , addClicked : msg
    , moreClicked : ProjectId -> String -> msg
    , sorted : List Project -> msg
    }
    -> System msg
system { toMsg, addClicked, moreClicked, sorted } =
    let
        config : Config msg
        config =
            { moreClicked = moreClicked
            , ep =
                { toggled = toMsg Toggled
                , title = "Projects"
                , secondary = { iconName = "add", action = addClicked }
                }
            , dndSystem = dndSystem
            }

        dndSystem : DND.System Project msg
        dndSystem =
            DND.system { toMsg = toMsg << DNDList, sorted = sorted }

        update : Msg -> Ret ProjectPanel msg -> Ret ProjectPanel msg
        update message =
            case message of
                DNDList msg ->
                    Ret.updateSubF fields.dnd (Ret.liftElmUpdate config.dndSystem.update) msg

                Toggled ->
                    Ret.mapSubF fields.collapsible EP.toggle
    in
    { initial =
        { collapsible = EP.expanded
        , dnd = dndSystem.initial
        }
    , subscriptions = fields.dnd.get >> dndSystem.subscriptions
    , update = update
    , view = view config
    , viewGhost = viewGhost
    }



--


type alias ProjectPanel =
    { collapsible : Collapsible
    , dnd : DNDList Project
    }


type alias Fields =
    { collapsible : Lens Collapsible ProjectPanel
    , dnd : Lens DNDProjectModel ProjectPanel
    }


fields : Fields
fields =
    { collapsible = Lens.fromTuple ( .collapsible, \s b -> { b | collapsible = s } )
    , dnd = Lens.fromTuple ( .dnd, \s b -> { b | dnd = s } )
    }



-- PROJECT PANEL UPDATE


type alias Config msg =
    { moreClicked : ProjectId -> String -> msg
    , ep : EP.Config msg
    , dndSystem : DND.System Project msg
    }


type Msg
    = DNDList (DND.Msg Project)
    | Toggled


type alias DNDProjectMsg =
    DND.Msg Project


type alias DNDProjectModel =
    DNDList Project


viewGhost : ProjectPanel -> List (Html msg)
viewGhost { dnd } =
    case DND.ghost dnd of
        Just ( style, project ) ->
            [ viewItem
                { itemAttrs = []
                , itemStyles = [ style ]
                , handleAttrs = []
                , moreAttrs = []
                }
                project
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
            config.dndSystem.view projectList dnd
    in
    List.map
        (\project ->
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
                project
        )
        items


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

        href =
            Route.projectHref project
    in
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
