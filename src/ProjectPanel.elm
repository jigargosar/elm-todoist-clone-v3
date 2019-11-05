module ProjectPanel exposing
    ( Config
    , Msg
    , ProjectPanel
    , initial
    , onToggle
    , subscriptions
    , update
    , view
    , viewGhost
    )

import Basics.More exposing (msgToCmd)
import Css
import DNDList
import Html.Styled as H exposing (Attribute, Html, a, button, div, i, text)
import Html.Styled.Attributes as A exposing (class, css, href)
import Html.Styled.Events exposing (onClick)
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Px
import Route
import Styles exposing (..)
import UI


type ProjectPanel
    = ProjectPanel { collapsed : Bool, dnd : DNDList.Model Project }


initial : ProjectPanel
initial =
    ProjectPanel { collapsed = False, dnd = DNDList.initial }



-- PROJECT PANEL UPDATE


subscriptions : ToMsg msg -> Config msg -> ProjectPanel -> Sub msg
subscriptions toMsg _ (ProjectPanel { dnd }) =
    DNDList.subscriptions dndConfig dnd
        |> Sub.map toMsg


type alias Config msg =
    { addClicked : msg
    , moreClicked : ProjectId -> String -> msg
    , sorted : List Project -> msg
    }


map : ({ collapsed : Bool, dnd : DNDList.Model Project } -> { collapsed : Bool, dnd : DNDList.Model Project }) -> ProjectPanel -> ProjectPanel
map func =
    unwrap >> func >> ProjectPanel


unwrap (ProjectPanel state) =
    state


onToggle : ProjectPanel -> ProjectPanel
onToggle =
    map (\model -> { model | collapsed = not model.collapsed })


type Msg
    = Toggled
    | DNDList (DNDList.Msg Project)
    | Sorted (List Project)
    | MoreClicked ProjectId String


type alias ToMsg msg =
    Msg -> msg


dndConfig =
    { toMsg = DNDList, sorted = Sorted }


update : ToMsg msg -> Config msg -> Msg -> ProjectPanel -> ( ProjectPanel, Cmd msg )
update toMsg config message model =
    case message of
        Toggled ->
            ( onToggle model, Cmd.none )

        DNDList msg ->
            DNDList.update dndConfig
                msg
                (unwrap model |> .dnd)
                |> Tuple.mapBoth (\dnd -> map (\state -> { state | dnd = dnd }) model)
                    (Cmd.map toMsg)

        Sorted projectList ->
            ( model, config.sorted projectList |> msgToCmd )

        MoreClicked projectId domId ->
            ( model, config.moreClicked projectId domId |> msgToCmd )


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


view : ToMsg msg -> Config msg -> List Project -> ProjectPanel -> List (Html msg)
view toMsg config projectList (ProjectPanel model) =
    UI.viewExpansionPanel
        { toggled = toMsg Toggled
        , title = "Projects"
        , collapsed = model.collapsed
        , secondary = { iconName = "add", action = config.addClicked }
        }
        (\_ ->
            viewItems toMsg
                config
                projectList
                model.dnd
        )


viewItems : ToMsg msg -> Config msg -> List Project -> DNDList.Model Project -> List (Html msg)
viewItems toMsg _ projectList dndList =
    let
        { dragStartAttrs, dragOverAttrs, isBeingDragged, items } =
            DNDList.view dndConfig projectList dndList
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
                    , onClick (MoreClicked (Project.id project) moreDomId)
                    ]
                }
                project
                |> H.map toMsg
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
