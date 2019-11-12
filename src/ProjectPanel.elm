module ProjectPanel exposing
    ( Config
    , Msg(..)
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
import DrawerUI
import ExpansionPanel as EP exposing (Collapsible)
import Html.Styled exposing (Attribute, Html, text)
import Html.Styled.Attributes as A exposing (href)
import Html.Styled.Events exposing (onClick)
import Lens exposing (Lens)
import PanelsHelp
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Ret exposing (Ret)
import Route
import Styles exposing (..)


type alias ProjectPanel =
    { collapsible : Collapsible
    , dnd : DNDList Project
    }


initial : ProjectPanel
initial =
    { collapsible = EP.expanded
    , dnd = DND.initial
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


type alias Config msg =
    { moreClicked : ProjectId -> String -> msg
    , ep : EP.Config msg
    , dnd : DND.Config Project msg
    , dndSys : DND.System Project msg
    }


createConfig :
    (Msg -> msg)
    ->
        { addClicked : msg
        , moreClicked : ProjectId -> String -> msg
        , sorted : List Project -> msg
        }
    -> Config msg
createConfig toMsg { addClicked, moreClicked, sorted } =
    let
        dndConfig =
            { toMsg = toMsg << DNDList, sorted = sorted }
    in
    { moreClicked = moreClicked
    , dnd = dndConfig
    , dndSys = DND.system dndConfig
    , ep =
        { toggled = toMsg Toggled
        , title = "Projects"
        , secondary = { iconName = "add", action = addClicked }
        }
    }


type Msg
    = DNDList (DND.Msg Project)
    | Toggled


type alias DNDProjectMsg =
    DND.Msg Project


type alias DNDProjectModel =
    DNDList Project


subscriptions : Config msg -> ProjectPanel -> Sub msg
subscriptions { dndSys } model =
    dndSys.subscriptions model.dnd


update : Config msg -> Msg -> ProjectPanel -> Ret ProjectPanel msg
update { dndSys } message model =
    case message of
        DNDList msg ->
            Ret.updateSub fields.dnd dndSys.update msg model

        Toggled ->
            ( Lens.over fields.collapsible EP.toggle model, Cmd.none )


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
                viewItems config projectList state.dnd
            )
            state.collapsible


viewItems : Config msg -> List Project -> DNDList Project -> List (Html msg)
viewItems { dndSys, moreClicked } projectList dnd =
    let
        { dragStartAttrs, dragOverAttrs, isBeingDragged, items } =
            dndSys.view projectList dnd
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
                    , onClick (moreClicked (Project.id project) moreDomId)
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
