module FilterPanel exposing
    ( Config
    , FilterPanel
    , initial
    , onDNDMsg
    , onToggle
    , subscriptions
    , view
    , viewGhost
    )

import Css
import DNDList
import Filter exposing (Filter)
import FilterId exposing (FilterId)
import Html.Styled exposing (Attribute, Html, a, button, div, i, text)
import Html.Styled.Attributes as A exposing (class, css, href)
import Html.Styled.Events exposing (onClick)
import Px
import Route
import Styles exposing (..)
import UI


type alias FilterPanel =
    { collapsed : Bool, dnd : DNDList.Model Filter }


initial : FilterPanel
initial =
    { collapsed = False, dnd = DNDList.initial }



-- FILTER PANEL UPDATE


subscriptions : Config msg -> FilterPanel -> Sub msg
subscriptions config { dnd } =
    DNDList.subscriptions config.dndConfig dnd


type alias Config msg =
    { toggled : msg
    , addClicked : msg
    , moreClicked : FilterId -> String -> msg
    , dndConfig : DNDList.Config Filter msg
    }


onToggle : FilterPanel -> FilterPanel
onToggle model =
    { model | collapsed = not model.collapsed }


onDNDMsg :
    Config msg
    -> DNDList.Msg Filter
    -> FilterPanel
    -> ( FilterPanel, Cmd msg )
onDNDMsg config msg model =
    DNDList.update config.dndConfig
        msg
        model.dnd
        |> Tuple.mapFirst (\dnd -> { model | dnd = dnd })


view : Config msg -> List Filter -> FilterPanel -> List (Html msg)
view config filterList model =
    let
        viewHeader =
            UI.viewExpansionPanelHeader
                { toggled = config.toggled
                , title = "Filters"
                , isExpanded = not model.collapsed
                , secondary = { iconName = "add", action = config.addClicked }
                }
    in
    viewHeader :: viewItems config filterList model.dnd


viewGhost : FilterPanel -> List (Html msg)
viewGhost { dnd } =
    case DNDList.ghost dnd of
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


viewItems : Config msg -> List Filter -> DNDList.Model Filter -> List (Html msg)
viewItems config filterList dndList =
    let
        { dragStartAttrs, dragOverAttrs, isBeingDragged, items } =
            DNDList.view config.dndConfig filterList dndList
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

        filterId =
            Filter.id filter

        href =
            Route.href (Route.Filter filterId)
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
