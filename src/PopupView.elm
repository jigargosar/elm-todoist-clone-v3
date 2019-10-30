module PopupView exposing
    ( FilterMenuItem(..)
    , LabelMenuItem(..)
    , ProjectMenuItem(..)
    , container
    , filterContent
    , labelContent
    , projectContent
    )

import Css
import Html.Styled as H exposing (Html, div, text)
import Html.Styled.Attributes as A exposing (class, css)
import Html.Styled.Events as E
import Json.Decode as JD
import Popper exposing (Popper)
import Styles exposing (..)


container : { onClose : msg, noOp : msg } -> List (Html msg) -> Popper -> List (Html msg)
container config content popper =
    [ div
        [ css
            [ Styles.fixed
            , Styles.absFill
            , Styles.flex
            , Styles.itemsCenter
            , Styles.justifyCenter
            , Styles.bg (Css.hsla 0 0 0 0.2)

            --                 , Styles.bg (Css.hsla 0 1 1 0.6)
            , Styles.z_ 10
            ]
        , E.onClick config.onClose
        ]
        [ div
            [ let
                popperStyles =
                    Popper.styles popper
              in
              css
                [ Styles.bgWhite
                , Styles.bor 3
                , Styles.batch popperStyles
                , pv 2
                ]
            , A.id "rootPopup"
            , E.stopPropagationOn "click" (JD.succeed ( config.noOp, True ))
            , A.class "shadow-1"
            ]
            content
        ]
    ]


type alias MenuItemConfig msg =
    { iconName : String, title : String, action : msg }


type ProjectMenuItem
    = AddProjectAbove
    | AddProjectBelow
    | EditProject
    | ShareProject
    | AddToFavorites
    | EmailTasksToThisProject
    | ProjectCalendarFeed
    | ArchiveProject
    | DeleteProject


projectContent : List (Html ProjectMenuItem)
projectContent =
    [ viewProjectMenuItem AddProjectAbove
    , viewProjectMenuItem AddProjectBelow
    , viewDivider
    , viewProjectMenuItem EditProject
    , viewProjectMenuItem ShareProject
    , viewProjectMenuItem AddToFavorites
    , viewDivider
    , viewProjectMenuItem EmailTasksToThisProject
    , viewProjectMenuItem ProjectCalendarFeed
    , viewDivider
    , viewProjectMenuItem ArchiveProject
    , viewProjectMenuItem DeleteProject
    ]


viewProjectMenuItem : ProjectMenuItem -> Html ProjectMenuItem
viewProjectMenuItem item =
    viewMenuItem (projectMenuItemConfig item)


projectMenuItemConfig : ProjectMenuItem -> MenuItemConfig ProjectMenuItem
projectMenuItemConfig action =
    let
        createHelp iconName title =
            MenuItemConfig iconName title action
    in
    case action of
        AddProjectAbove ->
            createHelp "arrow_upward" "Add Project above"

        AddProjectBelow ->
            createHelp "arrow_downward" "Add Project below"

        EditProject ->
            createHelp "edit" "Edit project"

        ShareProject ->
            createHelp "person_add" "Share project"

        AddToFavorites ->
            createHelp "favorite" "Add to favorites"

        EmailTasksToThisProject ->
            createHelp "email" "Email tasks to this project"

        ProjectCalendarFeed ->
            createHelp "list" "Project calendar feed"

        ArchiveProject ->
            createHelp "archive" "Archive"

        DeleteProject ->
            createHelp "delete" "Delete project"


type LabelMenuItem
    = EditLabel


labelContent : List (Html LabelMenuItem)
labelContent =
    [ viewLabelMenuItem EditLabel
    ]


viewLabelMenuItem : LabelMenuItem -> Html LabelMenuItem
viewLabelMenuItem item =
    viewMenuItem (labelMenuItemConfig item)


labelMenuItemConfig : LabelMenuItem -> MenuItemConfig LabelMenuItem
labelMenuItemConfig action =
    let
        createHelp iconName title =
            MenuItemConfig iconName title action
    in
    case action of
        EditLabel ->
            createHelp "edit" "Edit label"


type FilterMenuItem
    = EditFilter


filterContent : List (Html FilterMenuItem)
filterContent =
    [ viewFilterMenuItem EditFilter
    ]


viewFilterMenuItem : FilterMenuItem -> Html FilterMenuItem
viewFilterMenuItem item =
    viewMenuItem (filterMenuItemConfig item)


filterMenuItemConfig : FilterMenuItem -> MenuItemConfig FilterMenuItem
filterMenuItemConfig action =
    let
        createHelp iconName title =
            MenuItemConfig iconName title action
    in
    case action of
        EditFilter ->
            createHelp "edit" "Edit filter"


viewMenuItem : MenuItemConfig msg -> Html msg
viewMenuItem item =
    let
        { iconName, title, action } =
            item
    in
    div
        [ css [ flex, ph 2, pointer, noSelection, hover [ bgGrayL 0.95 ] ]
        , E.onClick action
        ]
        [ H.i
            [ css [ pv 1, ph 1, pointer ]
            , class "material-icons"
            ]
            [ text iconName ]
        , div [ css [ pv 1, ph 2, flex, flexGrow1, itemsCenter ] ] [ H.text title ]
        ]


viewDivider =
    div
        [ css
            [ bo_b
            , boColor <| grayL 0.9
            , Css.marginTop <| Css.px 8
            , Css.marginBottom <| Css.px 8
            ]
        ]
        []
