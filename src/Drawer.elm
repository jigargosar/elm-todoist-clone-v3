module Drawer exposing
    ( Panel(..)
    , PanelItemConfig
    , PanelItemId(..)
    , PanelMsg(..)
    , filterPanelItemConfig
    , labelPanelItemConfig
    , panelTitle
    , prefixNavItemsView
    , projectPanelItemConfig
    , viewPanel
    , viewPanelItemGhost
    , viewPanelItems
    , viewSimpleNavItem
    )

import Css
import Css.Transitions as Transitions exposing (transition)
import Drag exposing (Drag)
import DrawerItem as DI
import Filter exposing (Filter)
import FilterId exposing (FilterId)
import Html.Styled exposing (..)
import Html.Styled.Attributes as A exposing (class, css)
import Html.Styled.Events as E
import Json.Decode as JD
import Label exposing (Label)
import LabelId exposing (LabelId)
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Route
import StyleAttrs as SA exposing (StyleAttrs)
import Styles exposing (..)


prefixNavItemsView : List (Html msg)
prefixNavItemsView =
    [ viewSimpleNavItem (Route.href Route.Inbox) "Inbox" "inbox"
    , viewSimpleNavItem (Route.href Route.Inbox) "Today" "calendar_today"
    , viewSimpleNavItem (Route.href Route.Inbox) "Next 7 Days" "view_week"
    ]


type Panel
    = Projects
    | Labels
    | Filters


panelTitle : Panel -> String
panelTitle panel =
    case panel of
        Projects ->
            "Projects"

        Labels ->
            "Labels"

        Filters ->
            "Filters"


type PanelMsg
    = Toggle
    | Add
    | DragMsg Drag.Msg
    | DragComplete Drag.Info
    | More String PanelItemId


viewPanel : Panel -> Bool -> (() -> List (Html PanelMsg)) -> List (Html PanelMsg)
viewPanel panel isExpanded lazyContentView =
    let
        title =
            panelTitle panel
    in
    viewPanelHeader title isExpanded
        :: (if isExpanded then
                lazyContentView ()

            else
                []
           )


viewPanelHeader :
    String
    -> Bool
    -> Html PanelMsg
viewPanelHeader title isExpanded =
    let
        isCollapsed =
            not isExpanded

        iBtnStyle =
            batch [ btnReset, pointer ]
    in
    div
        [ css [ bo_b, boc (grayL 0.9), flex, hover [ bgGrayL 0.95 ] ] ]
        [ button
            [ css [ iBtnStyle, pa 1, flexGrow1 ], E.onClick Toggle ]
            [ span
                [ css
                    [ c_grayL 0.6
                    , batch
                        [ styleIf isCollapsed [ Css.transforms [ Css.rotate (Css.deg -90) ] ]
                        , transition [ Transitions.transform 200 ]
                        ]
                    ]
                ]
                [ i [ class "material-icons" ] [ text "expand_more" ] ]
            , styled span [ bold, pa 1 ] [] [ text title ]
            ]
        , button
            [ css [ iBtnStyle, mr 3 ]
            , E.onClick Add
            ]
            [ i [ class "material-icons" ] [ text "add" ] ]
        ]


type PanelItemId
    = ProjectItemId ProjectId
    | LabelItemId LabelId
    | FilterItemId FilterId


type alias PanelItemConfig id item =
    { moreClicked : String -> id -> JD.Decoder PanelMsg
    , dragMsg : Drag.Msg -> PanelMsg
    , panelId : String
    , id : item -> id
    , idToString : id -> String
    , title : item -> String
    , route : item -> Route.Route
    , iconName : String
    , iconStyle : item -> Style
    }


moreClickedDecoder : (id -> PanelItemId) -> String -> id -> JD.Decoder PanelMsg
moreClickedDecoder panelItemId anchorId id =
    let
        kind =
            panelItemId id

        msg =
            More anchorId kind
    in
    JD.succeed msg


projectPanelItemConfig : PanelItemConfig ProjectId Project
projectPanelItemConfig =
    { moreClicked = moreClickedDecoder ProjectItemId
    , dragMsg = DragMsg
    , panelId = "project"
    , iconName = "folder"
    , id = Project.id
    , idToString = ProjectId.toString
    , title = Project.title
    , route = Project.id >> Route.Project
    , iconStyle = Styles.c_ << Project.cssColor
    }


labelPanelItemConfig : PanelItemConfig LabelId Label
labelPanelItemConfig =
    { moreClicked = moreClickedDecoder LabelItemId
    , dragMsg = DragMsg
    , panelId = "label"
    , id = Label.id
    , idToString = LabelId.toString
    , title = Label.title
    , route = Label.id >> Route.Label
    , iconName = "label"
    , iconStyle = Styles.c_ << Label.cssColor
    }


filterPanelItemConfig : PanelItemConfig FilterId Filter
filterPanelItemConfig =
    { moreClicked = moreClickedDecoder FilterItemId
    , dragMsg = DragMsg
    , panelId = "filter"
    , id = Filter.id
    , idToString = FilterId.toString
    , title = Filter.title
    , route = Filter.id >> Route.Filter
    , iconName = "filter_list"
    , iconStyle = Styles.c_ << Filter.cssColor
    }


viewPanelItems : PanelItemConfig id item -> List item -> Drag -> List (Html PanelMsg)
viewPanelItems config items drag =
    items
        |> Drag.rotate drag
        |> List.indexedMap (viewPanelItem config drag)


panelItemDomId : PanelItemConfig id item -> id -> String
panelItemDomId config id =
    "drawer-panel__ " ++ config.panelId ++ "__item__" ++ config.idToString id


panelItemMoreBtnDomId : PanelItemConfig id item -> id -> String
panelItemMoreBtnDomId config id =
    panelItemDomId config id ++ "__more-btn"


viewPanelItem :
    PanelItemConfig id item
    -> Drag
    -> Int
    -> item
    -> Html PanelMsg
viewPanelItem config drag idx item =
    let
        id =
            config.id item

        domId =
            panelItemDomId config id

        rootSA =
            let
                dragOverStyle =
                    Styles.styleIf (Drag.eqDragOverIdx idx drag) [ Css.opacity <| Css.zero ]
            in
            StyleAttrs
                [ hover [ bgGrayL 0.9 ]
                , noSelection
                , dragOverStyle
                ]
                (A.id domId :: Drag.dropEvents config.dragMsg idx drag)

        primaryIcon : { name : String, sa : StyleAttrs PanelMsg }
        primaryIcon =
            let
                dragEvents =
                    Drag.dragEvents config.dragMsg idx domId drag
            in
            { name = config.iconName
            , sa = StyleAttrs [ Css.cursor Css.move, config.iconStyle item ] dragEvents
            }

        link : { title : String, sa : StyleAttrs msg }
        link =
            { title = config.title item, sa = StyleAttrs [] [ Route.href <| config.route item ] }

        moreDomId =
            panelItemMoreBtnDomId config id

        moreSA : StyleAttrs PanelMsg
        moreSA =
            StyleAttrs [] [ A.id moreDomId, E.on "click" (config.moreClicked moreDomId id) ]
    in
    viewPanelItemHelp rootSA primaryIcon link moreSA


viewPanelItemGhost : PanelItemConfig id item -> List item -> Drag -> List (Html msg)
viewPanelItemGhost config items drag =
    Drag.ghostItemWithStyles items drag
        |> Maybe.map
            (\( ghostStyles, item ) ->
                let
                    icon =
                        { name = config.iconName, sa = SA.styles [ config.iconStyle item ] }

                    rootSA =
                        SA.styles ghostStyles

                    title =
                        config.title item
                in
                [ viewPanelItemGhostHelp rootSA icon title
                , node "style" [] [ text "body *{ cursor:move!important; }" ]
                ]
            )
        |> Maybe.withDefault []


viewSimpleNavItem : Attribute msg -> String -> String -> Html msg
viewSimpleNavItem href title iconName =
    viewSimpleNavItemHelp (StyleAttrs [] [ href ]) { name = iconName, sa = SA.none } title


viewSimpleNavItemHelp : StyleAttrs msg -> { a | name : String, sa : StyleAttrs msg } -> String -> Html msg
viewSimpleNavItemHelp rootSA icon title =
    DI.initLink rootSA
        |> DI.withPrimaryIcon icon.name icon.sa
        |> DI.withContentText title
        |> DI.render


viewPanelItemHelp :
    StyleAttrs msg
    -> { a | name : String, sa : StyleAttrs msg }
    -> { b | title : String, sa : StyleAttrs msg }
    -> StyleAttrs msg
    -> Html msg
viewPanelItemHelp rootSA icon linkContent moreSA =
    DI.init rootSA
        |> DI.withPrimaryIcon icon.name icon.sa
        |> DI.withContentAsLink linkContent.title linkContent.sa
        |> DI.withSecondaryMoreAction moreSA
        |> DI.render


viewPanelItemGhostHelp : StyleAttrs msg -> { a | name : String, sa : StyleAttrs msg } -> String -> Html msg
viewPanelItemGhostHelp rootSA icon title =
    DI.init rootSA
        |> DI.withPrimaryIcon icon.name icon.sa
        |> DI.withContentText title
        |> DI.render
