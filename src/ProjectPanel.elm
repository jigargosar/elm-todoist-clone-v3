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
import DNDList
import ExpansionPanel exposing (ExpansionPanel)
import Html.Styled exposing (Attribute, Html, a, button, div, i, text)
import Html.Styled.Attributes as A exposing (class, css, href)
import Html.Styled.Events exposing (onClick)
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Px
import Route
import Styles exposing (..)


type ProjectPanel
    = ProjectPanel State


type alias State =
    { expansionPanel : ExpansionPanel
    , dnd : DNDList.Model Project
    }


initial : ProjectPanel
initial =
    ProjectPanel
        { expansionPanel = ExpansionPanel.initial
        , dnd = DNDList.initial
        }



-- PROJECT PANEL UPDATE


subscriptions : Config msg -> ProjectPanel -> Sub msg
subscriptions config (ProjectPanel { dnd, expansionPanel }) =
    Sub.batch
        [ DNDList.subscriptions config.dnd dnd
        , ExpansionPanel.subscriptions config.expansionPanel expansionPanel
        ]


type alias Config msg =
    { moreClicked : ProjectId -> String -> msg
    , dnd : DNDList.Config Project msg
    , expansionPanel : ExpansionPanel.Config msg
    }


createConfig :
    ToMsg msg
    ->
        { addClicked : msg
        , moreClicked : ProjectId -> String -> msg
        , sorted : List Project -> msg
        }
    -> Config msg
createConfig toMsg { addClicked, moreClicked, sorted } =
    { moreClicked = moreClicked
    , dnd = { toMsg = toMsg << DNDList, sorted = sorted }
    , expansionPanel =
        ExpansionPanel.createConfig (toMsg << ExpansionPanel)
            { title = "Projects", secondary = { iconName = "add", action = addClicked } }
    }


map : (State -> State) -> ProjectPanel -> ProjectPanel
map func =
    unwrap >> func >> ProjectPanel


unwrap : ProjectPanel -> State
unwrap (ProjectPanel state) =
    state


type Msg
    = DNDList (DNDList.Msg Project)
    | ExpansionPanel ExpansionPanel.Msg


type alias ToMsg msg =
    Msg -> msg


update : Config msg -> Msg -> ProjectPanel -> ( ProjectPanel, Cmd msg )
update config message model =
    case message of
        DNDList msg ->
            DNDList.update config.dnd msg (unwrap model |> .dnd)
                |> Tuple.mapFirst (\dnd -> map (\state -> { state | dnd = dnd }) model)

        ExpansionPanel msg ->
            ExpansionPanel.update config.expansionPanel msg (unwrap model |> .expansionPanel)
                |> Tuple.mapFirst (\expansionPanel -> map (\state -> { state | expansionPanel = expansionPanel }) model)


viewGhost : ProjectPanel -> List (Html msg)
viewGhost (ProjectPanel { dnd }) =
    case DNDList.ghost dnd of
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
view config projectList (ProjectPanel state) =
    ExpansionPanel.view config.expansionPanel
        (\_ ->
            viewItems config
                projectList
                state.dnd
        )
        state.expansionPanel


viewItems : Config msg -> List Project -> DNDList.Model Project -> List (Html msg)
viewItems config projectList dndList =
    let
        { dragStartAttrs, dragOverAttrs, isBeingDragged, items } =
            DNDList.view config.dnd projectList dndList
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
