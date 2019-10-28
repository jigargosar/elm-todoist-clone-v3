module LabelPanel exposing
    ( Config
    , LabelPanel
    , initial
    , onDNDMsg
    , onToggle
    , subscriptions
    , view
    , viewGhost
    )

import Css
import DNDList
import Html.Styled exposing (Attribute, Html, a, button, div, i, text)
import Html.Styled.Attributes as A exposing (class, css, href)
import Html.Styled.Events exposing (onClick)
import Label exposing (Label)
import LabelId exposing (LabelId)
import Px
import Route
import Styles exposing (..)
import UI


type alias LabelPanel =
    { collapsed : Bool, dnd : DNDList.Model Label }


initial : LabelPanel
initial =
    { collapsed = False, dnd = DNDList.initial }



-- LABEL PANEL UPDATE


subscriptions : Config msg -> LabelPanel -> Sub msg
subscriptions config { dnd } =
    DNDList.subscriptions config.dndConfig dnd


type alias Config msg =
    { toggled : msg
    , addClicked : msg
    , moreClicked : LabelId -> String -> msg
    , dndConfig : DNDList.Config Label msg
    }


onToggle : LabelPanel -> LabelPanel
onToggle model =
    { model | collapsed = not model.collapsed }


onDNDMsg :
    Config msg
    -> DNDList.Msg Label
    -> LabelPanel
    -> ( LabelPanel, Cmd msg )
onDNDMsg config msg model =
    DNDList.update config.dndConfig
        msg
        model.dnd
        |> Tuple.mapFirst (\dnd -> { model | dnd = dnd })


view : Config msg -> List Label -> LabelPanel -> List (Html msg)
view config labelList model =
    let
        viewHeader =
            UI.viewExpansionPanelHeader
                { toggled = config.toggled
                , title = "Labels"
                , isExpanded = not model.collapsed
                , secondary = { iconName = "add", action = config.addClicked }
                }
    in
    viewHeader :: viewItems config labelList model.dnd


viewGhost : LabelPanel -> List (Html msg)
viewGhost { dnd } =
    case DNDList.ghost dnd of
        Just ( style, label ) ->
            [ viewItem
                { itemAttrs = []
                , itemStyles = [ style ]
                , handleAttrs = []
                , moreAttrs = []
                }
                label
            ]

        Nothing ->
            []


viewItems : Config msg -> List Label -> DNDList.Model Label -> List (Html msg)
viewItems config labelList dndList =
    let
        { dragStartAttrs, dragOverAttrs, isBeingDragged, items } =
            DNDList.view config.dndConfig labelList dndList
    in
    List.map
        (\label ->
            let
                domId =
                    itemDomId label

                moreDomId =
                    domId ++ "__more-btn"
            in
            viewItem
                { itemAttrs = A.id domId :: dragOverAttrs label
                , itemStyles = [ styleIf (isBeingDragged label) [ Css.opacity <| Css.zero ] ]
                , handleAttrs = dragStartAttrs label domId
                , moreAttrs =
                    [ A.id moreDomId
                    , onClick (config.moreClicked (Label.id label) moreDomId)
                    ]
                }
                label
        )
        items


itemDomId : Label -> String
itemDomId label =
    "label-panel-item__" ++ (Label.id label |> LabelId.toString)


viewItem : ItemProps msg -> Label -> Html msg
viewItem { itemAttrs, itemStyles, handleAttrs, moreAttrs } label =
    let
        title =
            Label.title label

        iconColor =
            Label.cssColor label

        labelId =
            Label.id label

        href =
            Route.href (Route.Label labelId)
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
