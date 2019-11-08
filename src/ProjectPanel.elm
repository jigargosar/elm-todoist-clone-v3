module ProjectPanel exposing
    ( Msg
    , ProjectPanel
    , System
    , subscriptions
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
import Ret exposing (Ret, RetCmd)
import Route
import Styles exposing (..)



-- SYSTEM


type alias System msg =
    { initial : ProjectPanel
    , subscriptions : ProjectPanel -> Sub msg
    , update : Msg -> ProjectPanel -> ( ProjectPanel, Cmd msg )
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
        config =
            { moreClicked = moreClicked
            , dnd = { toMsg = toMsg << DNDList, sorted = sorted }
            , ep =
                { toggled = toMsg Toggled
                , title = "Projects"
                , secondary = { iconName = "add", action = addClicked }
                }
            }
    in
    { initial = initial
    , subscriptions = subscriptions config
    , update = Ret.toElmUpdate (update2 config)
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


initial : ProjectPanel
    initial =
    { collapsible = EP.expanded
    , dnd = DND.initial
    }


fields : Fields
fields =
    { collapsible = Lens.fromTuple ( .collapsible, \s b -> { b | collapsible = s } )
    , dnd = Lens.fromTuple ( .dnd, \s b -> { b | dnd = s } )
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
    }


type Msg
    = DNDList (DND.Msg Project)
    | Toggled


type alias DNDProjectMsg =
    DND.Msg Project


type alias DNDProjectModel =
    DNDList Project


dndUpdate : Config msg -> DNDProjectMsg -> RetCmd DNDProjectModel msg -> RetCmd DNDProjectModel msg
dndUpdate config =
    Ret.liftUpdate (DND.update config.dnd)


update2 : Config msg -> Msg -> RetCmd ProjectPanel msg -> RetCmd ProjectPanel msg
update2 config message =
    case message of
        DNDList msg ->
            Ret.updateSub fields.dnd (dndUpdate config) msg

        Toggled ->
            Ret.mapSub fields.collapsible EP.toggle


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
            DND.view config.dnd projectList dnd
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
