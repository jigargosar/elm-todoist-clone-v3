module Drawer exposing
    ( Panel(..)
    , PanelConfig
    , PanelItemConfig
    , PanelItemId(..)
    , getExpansionPanelConfig
    , panelTitle
    , prefixNavItemsView
    , viewPanel
    , viewPanelItemGhost
    , viewPanelItems
    , viewSimpleNavItem
    )

import Css
import Drag exposing (Drag)
import DrawerItem as DI
import ExpansionPanelUI
import FilterId exposing (FilterId)
import Html.Styled exposing (..)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as JD
import LabelId exposing (LabelId)
import ProjectId exposing (ProjectId)
import Route
import StyleAttrs as SA exposing (StyleAttrs)
import Styles exposing (..)
import View exposing (View)


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


type alias PanelConfig msg =
    { toggle : Panel -> msg
    , add : Panel -> msg
    }


getExpansionPanelConfig : Panel -> PanelConfig msg -> ExpansionPanelUI.Config msg
getExpansionPanelConfig panel { toggle, add } =
    { toggle = toggle panel, add = add panel }


viewPanel : PanelConfig msg -> Panel -> Bool -> (() -> List (Html msg)) -> List (Html msg)
viewPanel config panel isExpanded lazyContentView =
    let
        title =
            panelTitle panel

        toggleMsg =
            getExpansionPanelConfig panel config
    in
    ExpansionPanelUI.view toggleMsg title isExpanded lazyContentView


type PanelItemId
    = ProjectItemId ProjectId
    | LabelItemId LabelId
    | FilterItemId FilterId


type alias PanelItemConfig id item msg =
    { moreClicked : String -> id -> JD.Decoder msg
    , dragMsg : Drag.Msg -> msg
    , panelId : String
    , id : item -> id
    , idToString : id -> String
    , title : item -> String
    , route : item -> Route.Route
    , iconName : String
    , iconStyle : item -> Style
    }


viewPanelItems : PanelItemConfig id item msg -> List item -> Drag -> List (Html msg)
viewPanelItems config items drag =
    items
        |> Drag.rotate drag
        |> List.indexedMap (viewPanelItem config drag)


panelItemDomId : PanelItemConfig id item msg -> id -> String
panelItemDomId config id =
    "drawer-panel__ " ++ config.panelId ++ "__item__" ++ config.idToString id


panelItemMoreBtnDomId : PanelItemConfig id item msg -> id -> String
panelItemMoreBtnDomId config id =
    panelItemDomId config id ++ "__more-btn"


viewPanelItem :
    PanelItemConfig id item msg
    -> Drag
    -> Int
    -> item
    -> Html msg
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

        primaryIcon : { name : String, sa : StyleAttrs msg }
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

        moreSA : StyleAttrs msg
        moreSA =
            StyleAttrs [] [ A.id moreDomId, E.on "click" (config.moreClicked moreDomId id) ]
    in
    viewPanelItemHelp rootSA primaryIcon link moreSA


viewPanelItemGhost : PanelItemConfig id item msg -> List item -> Drag -> List (Html msg)
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
