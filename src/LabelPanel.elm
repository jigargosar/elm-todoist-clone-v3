module LabelPanel exposing
    ( Config
    , LabelPanel
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
import Html.Styled exposing (Attribute, Html, a, button, div, i, text)
import Html.Styled.Attributes as A exposing (class, css, href)
import Html.Styled.Events exposing (onClick)
import Label exposing (Label)
import LabelId exposing (LabelId)
import PanelsHelp
import Px
import Ret exposing (Ret)
import Route
import Styles exposing (..)
import UI.Icon as Icon


type LabelPanel
    = LabelPanel State


type alias State =
    { collapsible : Collapsible
    , dnd : DNDList Label
    }


initial : LabelPanel
initial =
    LabelPanel
        { collapsible = EP.expanded
        , dnd = DND.initial
        }



-- LABEL PANEL UPDATE


subscriptions : Config msg -> LabelPanel -> Sub msg
subscriptions config (LabelPanel { dnd }) =
    Sub.batch
        [ DND.subscriptions config.dnd dnd
        ]


type alias Config msg =
    { moreClicked : LabelId -> String -> msg
    , dnd : DND.Config Label msg
    , ep : EP.Config msg
    }


createConfig :
    { toMsg : Msg -> msg
    , addClicked : msg
    , moreClicked : LabelId -> String -> msg
    , sorted : List Label -> msg
    }
    -> Config msg
createConfig { toMsg, addClicked, moreClicked, sorted } =
    let
        ep =
            { toggled = toMsg Toggled
            , title = "Labels"
            , secondary = { iconName = "add", action = addClicked }
            }
    in
    { moreClicked = moreClicked
    , dnd = { toMsg = toMsg << DNDList, sorted = sorted }
    , ep = ep
    }


type Msg
    = DNDList (DND.Msg Label)
    | Toggled


elmUpdate : Config msg -> Msg -> LabelPanel -> ( LabelPanel, Cmd msg )
elmUpdate config message (LabelPanel state) =
    case message of
        DNDList msg ->
            DND.update config.dnd msg state.dnd
                |> Tuple.mapFirst (\dnd -> LabelPanel { state | dnd = dnd })

        Toggled ->
            ( LabelPanel { state | collapsible = EP.toggle state.collapsible }, Cmd.none )


update : Config msg -> Msg -> Ret LabelPanel msg -> Ret LabelPanel msg
update config =
    Ret.fromElmUpdate (elmUpdate config)


viewGhost : LabelPanel -> List (Html msg)
viewGhost (LabelPanel { dnd }) =
    case DND.ghost dnd of
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


view : Config msg -> List Label -> LabelPanel -> List (Html msg)
view config labelList (LabelPanel state) =
    EP.view config.ep
        (\_ ->
            viewItems config
                labelList
                state.dnd
        )
        state.collapsible


viewItems : Config msg -> List Label -> DNDList Label -> List (Html msg)
viewItems config labelList dndList =
    let
        { dragStartAttrs, dragOverAttrs, isBeingDragged, items } =
            DND.view config.dnd labelList dndList
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

        href =
            Route.labelHref label
    in
    div (css [ Px.pl 4, Px.pr (4 + 16), flex, batch itemStyles ] :: class "hover_parent" :: itemAttrs)
        [ Icon.view2 PanelsHelp.labelIcon
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
