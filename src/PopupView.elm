module PopupView exposing (..)

import Css
import Html.Styled as H exposing (Html, div, text)
import Html.Styled.Attributes as A exposing (class, css)
import Html.Styled.Events as E
import Json.Decode as JD
import Popper exposing (Popper)
import Styles exposing (..)
import View exposing (View)


mockPopupView : { onClose : msg, noOp : msg } -> List (Html msg) -> Popper -> View (Html msg)
mockPopupView config content popper =
    View.portal
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


type alias ProjectMenuItemConfig =
    { iconName : String, title : String }


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


projectContent : List (Html msg)
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


viewDivider =
    div
        [ css
            [ bo_b
            , boc <| grayL 0.9
            , Css.marginTop <| Css.px 8
            , Css.marginBottom <| Css.px 8
            ]
        ]
        []


viewProjectMenuItem item =
    let
        { iconName, title } =
            projectMenuItemConfig item
    in
    div [ css [ flex, ph 2 ] ]
        [ H.i
            [ css [ pv 1, ph 1, pointer ]
            , class "material-icons"
            ]
            [ text iconName ]
        , div [ css [ pv 1, ph 2, flex, flexGrow1, itemsCenter ] ] [ H.text title ]
        ]


projectMenuItemConfig item =
    case item of
        AddProjectAbove ->
            ProjectMenuItemConfig "arrow_upward" "Add Project above"

        AddProjectBelow ->
            ProjectMenuItemConfig "arrow_downward" "Add Project below"

        EditProject ->
            ProjectMenuItemConfig "edit" "Edit project"

        ShareProject ->
            ProjectMenuItemConfig "share" "Share project"

        AddToFavorites ->
            ProjectMenuItemConfig "favorite" "Add to favorites"

        EmailTasksToThisProject ->
            ProjectMenuItemConfig "email" "Email tasks to this project"

        ProjectCalendarFeed ->
            ProjectMenuItemConfig "list" "Project calendar feed"

        ArchiveProject ->
            ProjectMenuItemConfig "archive" "Archive"

        DeleteProject ->
            ProjectMenuItemConfig "delete" "Delete project"
