module ProjectPanel exposing
    ( Config
    , ProjectPanel
    , initial
    , onDNDMsg
    , onToggle
    , subscriptions
    , update
    , view
    , viewGhost
    )

import Basics.More exposing (msgToCmd)
import Css
import DNDList
import Html.Styled exposing (Attribute, Html, a, button, div, i, text)
import Html.Styled.Attributes as A exposing (class, css, href)
import Html.Styled.Events exposing (onClick)
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Px
import Route
import Styles exposing (..)
import UI


type alias ProjectPanel =
    { collapsed : Bool, dnd : DNDList.Model Project }


initial : ProjectPanel
initial =
    { collapsed = False, dnd = DNDList.initial }



-- PROJECT PANEL UPDATE


subscriptions : Config msg -> ProjectPanel -> Sub msg
subscriptions config { dnd } =
    DNDList.subscriptions config.dndConfig dnd


type alias Config msg =
    { toggled : msg
    , addClicked : msg
    , moreClicked : ProjectId -> String -> msg
    , dndConfig : DNDList.Config Project msg
    }


onToggle : ProjectPanel -> ProjectPanel
onToggle model =
    { model | collapsed = not model.collapsed }


onDNDMsg :
    Config msg
    -> DNDList.Msg Project
    -> ProjectPanel
    -> ( ProjectPanel, Cmd msg )
onDNDMsg config msg model =
    DNDList.update config.dndConfig
        msg
        model.dnd
        |> Tuple.mapFirst (\dnd -> { model | dnd = dnd })


type Msg
    = Toggle
    | DNDList (DNDList.Msg Project)
    | Sorted (List Project)


type alias ToMsg msg =
    Msg -> msg


update : ToMsg msg -> Config msg -> Msg -> ProjectPanel -> ( ProjectPanel, Cmd msg )
update toMsg config message model =
    case message of
        Toggle ->
            ( onToggle model, Cmd.none )

        DNDList msg ->
            DNDList.update { toMsg = DNDList, sorted = Sorted }
                msg
                model.dnd
                |> Tuple.mapBoth (\dnd -> { model | dnd = dnd })
                    (Cmd.map toMsg)

        Sorted projectList ->
            ( model, config.dndConfig.sorted projectList |> msgToCmd )


viewGhost : ProjectPanel -> List (Html msg)
viewGhost { dnd } =
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
view config projectList model =
    UI.viewExpansionPanel
        { toggled = config.toggled
        , title = "Projects"
        , collapsed = model.collapsed
        , secondary = { iconName = "add", action = config.addClicked }
        }
        (\_ ->
            viewItems config
                projectList
                model.dnd
        )


viewItems : Config msg -> List Project -> DNDList.Model Project -> List (Html msg)
viewItems config projectList dndList =
    let
        { dragStartAttrs, dragOverAttrs, isBeingDragged, items } =
            DNDList.view config.dndConfig projectList dndList
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
