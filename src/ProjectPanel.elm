module ProjectPanel exposing
    ( Msg
    , ProjectPanel
    , System
    , system
    )

import Css
import DNDList as DND exposing (DNDList)
import DrawerUI
import ExpansionPanel as EP exposing (Collapsible)
import Html.Styled as H exposing (Attribute, Html, text)
import Html.Styled.Attributes as A exposing (href)
import Html.Styled.Events exposing (onClick)
import Lens exposing (Lens)
import PanelsHelp
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Ret exposing (Ret)
import Route
import Styles exposing (..)



-- SYSTEM


type alias System msg =
    { initial : ProjectPanel
    , subscriptions : ProjectPanel -> Sub msg
    , update : Msg -> ProjectPanel -> Ret ProjectPanel msg
    , view : List Project -> ProjectPanel -> List (Html msg)
    , viewGhost : ProjectPanel -> List (Html msg)
    }


system : Config msg -> System msg
system config =
    { initial = initial
    , subscriptions = subscriptions config
    , update = update config
    , view = view config
    , viewGhost = viewGhost
    }


subscriptions : Config msg -> ProjectPanel -> Sub msg
subscriptions { toMsg } model =
    dndSystem.subscriptions model.dnd
        |> Sub.map toMsg



--


type alias ProjectPanel =
    { collapsible : Collapsible
    , dnd : DNDList Project
    }


initial : ProjectPanel
initial =
    { collapsible = EP.expanded
    , dnd = dndSystem.initial
    }


type alias Fields =
    { collapsible : Lens Collapsible ProjectPanel
    , dnd : Lens DNDProjectModel ProjectPanel
    }


fields : Fields
fields =
    { collapsible = Lens.fromTuple ( .collapsible, \s b -> { b | collapsible = s } )
    , dnd = Lens.fromTuple ( .dnd, \s b -> { b | dnd = s } )
    }



-- PROJECT PANEL UPDATE


type alias Config msg =
    { toMsg : Msg -> msg
    , addClicked : msg
    , moreClicked : ProjectId -> String -> msg
    , sorted : List Project -> msg
    }


dndSystem : DND.System Project Msg
dndSystem =
    DND.system { toMsg = DNDList, sorted = Sorted }


epConfig =
    { toggled = Toggled
    , title = "Projects"
    , secondary = { iconName = "add", action = AddClicked }
    }


type Msg
    = DNDList (DND.Msg Project)
    | Toggled
    | Sorted (List Project)
    | AddClicked
    | MoreClicked ProjectId String


type alias DNDProjectMsg =
    DND.Msg Project


type alias DNDProjectModel =
    DNDList Project


update : Config msg -> Msg -> ProjectPanel -> Ret ProjectPanel msg
update { toMsg, sorted, addClicked, moreClicked } message model =
    case message of
        DNDList msg ->
            Ret.updateSub fields.dnd dndSystem.update msg model
                |> Ret.mapCmd toMsg

        Toggled ->
            ( Lens.over fields.collapsible EP.toggle model, Cmd.none )

        Sorted projectList ->
            ( model, Ret.toCmd (sorted projectList) )

        AddClicked ->
            ( model, Ret.toCmd addClicked )

        MoreClicked projectId domId ->
            ( model, Ret.toCmd (moreClicked projectId domId) )


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
view { toMsg } projectList state =
    (EP.viewHeader epConfig state.collapsible
        :: EP.viewContent
            (\_ ->
                viewItems projectList state.dnd
            )
            state.collapsible
    )
        |> List.map (H.map toMsg)


viewItems : List Project -> DNDList Project -> List (Html Msg)
viewItems projectList dnd =
    let
        { dragStartAttrs, dragOverAttrs, isBeingDragged, items } =
            dndSystem.view projectList dnd
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
    DrawerUI.item itemStyles
        itemAttrs
        [ DrawerUI.dragHandle [ c_ iconColor ] handleAttrs PanelsHelp.projectIcon
        , DrawerUI.link [] [ href ] [ text title ]
        , DrawerUI.more moreAttrs
        ]


type alias ItemProps msg =
    { itemAttrs : List (Attribute msg)
    , itemStyles : List Style
    , handleAttrs : List (Attribute msg)
    , moreAttrs : List (Attribute msg)
    }
