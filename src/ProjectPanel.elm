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
import ExpansionPanel as EP exposing (Collapsible)
import Html.Styled exposing (Attribute, Html, a, button, div, i, text)
import Html.Styled.Attributes as A exposing (class, css, href)
import Html.Styled.Events exposing (onClick)
import PanelsHelp
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Px
import Route
import Style
import Styles exposing (..)
import UI.Icon as Icon


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
    { a
        | dnd :
            { b | toMsg : DND.Msg Project -> c }
        , moreClicked : ProjectId -> String -> c
    }
    -> List Project
    -> DNDList Project
    -> List (Html c)
viewItems config projectList dndList =
    let
        { dragStartAttrs, dragOverAttrs, isBeingDragged, items } =
            DND.view config.dnd projectList dndList
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
    div (css [ Px.pl 4, Px.pr (4 + 16), flex, batch itemStyles ] :: class "hover_parent" :: itemAttrs)
        [ Icon.view2 PanelsHelp.projectIcon
            (css [ Px.pa 4, Px.m2 4 0, cursorMove, c_ iconColor ]
                :: handleAttrs
            )
        , a [ css [ linkReset, Px.p2 8 4, lh 1.5, flexGrow1 ], href ] [ text title ]
        , button
            ([ css [ Style.btnLink, Style.btnAction ]
             , class "show_on_parent_hover"
             ]
                ++ moreAttrs
            )
            [ Icon.view Icon.MoreHorizontal
            ]
        ]


type alias ItemProps msg =
    { itemAttrs : List (Attribute msg)
    , itemStyles : List Style
    , handleAttrs : List (Attribute msg)
    , moreAttrs : List (Attribute msg)
    }
