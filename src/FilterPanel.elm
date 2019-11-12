module FilterPanel exposing
    ( Config
    , FilterPanel
    , Msg
    , createConfig
    , initial
    , subscriptions
    , update
    , view
    , viewGhost
    )

import Css
import DNDList as DND exposing (DNDList)
import ExpansionPanel as EP exposing (Collapsible)
import Filter exposing (Filter)
import FilterId exposing (FilterId)
import Html.Styled exposing (Attribute, Html, a, button, div, i, text)
import Html.Styled.Attributes as A exposing (class, css, href)
import Html.Styled.Events exposing (onClick)
import PanelsHelp
import Px
import Ret exposing (Ret)
import Route
import Styles exposing (..)
import UI.Icon as Icon


type FilterPanel
    = FilterPanel State


type alias State =
    { collapsible : Collapsible
    , dnd : DNDList Filter
    }


initial : FilterPanel
initial =
    FilterPanel
        { collapsible = EP.expanded
        , dnd = DND.initial
        }



-- FILTER PANEL UPDATE


subscriptions : Config msg -> FilterPanel -> Sub msg
subscriptions config (FilterPanel { dnd }) =
    Sub.batch
        [ DND.subscriptions config.dnd dnd
        ]


type alias Config msg =
    { moreClicked : FilterId -> String -> msg
    , dnd : DND.Config Filter msg
    , ep : EP.Config msg
    }


createConfig :
    { toMsg : Msg -> msg
    , addClicked : msg
    , moreClicked : FilterId -> String -> msg
    , sorted : List Filter -> msg
    }
    -> Config msg
createConfig { toMsg, addClicked, moreClicked, sorted } =
    let
        ep =
            { toggled = toMsg Toggled
            , title = "Filters"
            , secondary = { iconName = "add", action = addClicked }
            }
    in
    { moreClicked = moreClicked
    , dnd = { toMsg = toMsg << DNDList, sorted = sorted }
    , ep = ep
    }


type Msg
    = DNDList (DND.Msg Filter)
    | Toggled


update : Config msg -> Msg -> FilterPanel -> ( FilterPanel, Cmd msg )
update config message (FilterPanel state) =
    case message of
        DNDList msg ->
            DND.update config.dnd msg state.dnd
                |> Tuple.mapFirst (\dnd -> FilterPanel { state | dnd = dnd })

        Toggled ->
            ( FilterPanel { state | collapsible = EP.toggle state.collapsible }, Cmd.none )


viewGhost : FilterPanel -> List (Html msg)
viewGhost (FilterPanel { dnd }) =
    case DND.ghost dnd of
        Just ( style, filter ) ->
            [ viewItem
                { itemAttrs = []
                , itemStyles = [ style ]
                , handleAttrs = []
                , moreAttrs = []
                }
                filter
            ]

        Nothing ->
            []


view : Config msg -> List Filter -> FilterPanel -> List (Html msg)
view config filterList (FilterPanel state) =
    EP.view config.ep
        (\_ ->
            viewItems config
                filterList
                state.dnd
        )
        state.collapsible


viewItems : Config msg -> List Filter -> DNDList Filter -> List (Html msg)
viewItems config filterList dndList =
    let
        { dragStartAttrs, dragOverAttrs, isBeingDragged, items } =
            DND.view config.dnd filterList dndList
    in
    List.map
        (\filter ->
            let
                domId =
                    itemDomId filter

                moreDomId =
                    domId ++ "__more-btn"
            in
            viewItem
                { itemAttrs = A.id domId :: dragOverAttrs filter
                , itemStyles = [ styleIf (isBeingDragged filter) [ Css.opacity <| Css.zero ] ]
                , handleAttrs = dragStartAttrs filter domId
                , moreAttrs =
                    [ A.id moreDomId
                    , onClick (config.moreClicked (Filter.id filter) moreDomId)
                    ]
                }
                filter
        )
        items


itemDomId : Filter -> String
itemDomId filter =
    "filter-panel-item__" ++ (Filter.id filter |> FilterId.toString)


viewItem : ItemProps msg -> Filter -> Html msg
viewItem { itemAttrs, itemStyles, handleAttrs, moreAttrs } filter =
    let
        title =
            Filter.title filter

        iconColor =
            Filter.cssColor filter

        href =
            Route.filterHref filter
    in
    div (css [ Px.pl 4, Px.pr (4 + 16), flex, batch itemStyles ] :: class "hover_parent" :: itemAttrs)
        [ Icon.view2 PanelsHelp.filterIcon
            (css [ Px.pa 4, Px.m2 4 0, cursorMove, c_ iconColor ]
                :: handleAttrs
            )
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
