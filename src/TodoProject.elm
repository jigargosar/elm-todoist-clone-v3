module TodoProject exposing
    ( view
    , viewInboxTitle
    , viewProjectTitle
    )

import CColor exposing (CColor)
import Color exposing (Color)
import Css
import Html.Styled exposing (Attribute, Html, a, div, text)
import Html.Styled.Attributes exposing (css)
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Px
import Route
import Styles exposing (..)
import UI.Icon as Icon
import UI.IconButton as IconButton


inboxTitle =
    "Inbox"


inboxColor =
    CColor.toColor CColor.Charcoal


inboxHref =
    Route.inboxHref


title : Maybe Project -> String
title =
    Maybe.map Project.title >> Maybe.withDefault inboxTitle


color : Maybe Project -> Color
color =
    Maybe.map (Project.cColor >> CColor.toColor) >> Maybe.withDefault inboxColor


cssColor =
    color >> toCssColor


highContrastCssColor =
    color >> Color.highContrast >> toCssColor


href : Maybe Project -> Attribute msg
href =
    Maybe.map Route.projectHref >> Maybe.withDefault inboxHref


view : Maybe Project -> Html msg
view maybeProject =
    a
        [ css
            [ linkReset
            , ph 1
            , lh 1.5
            , Css.fontSize Css.small
            , bg (cssColor maybeProject)
            , fg (highContrastCssColor maybeProject)
            , boRad 2
            , hover [ underline, pointer ]
            ]
        , href maybeProject
        ]
        [ text <| title maybeProject ]


viewProjectTitle :
    { a | editClicked : ProjectId -> msg, noOp : msg }
    -> Project
    -> Html msg
viewProjectTitle { editClicked, noOp } project =
    let
        projectId =
            Project.id project
    in
    div [ css [ flex, Px.pt 8 ] ]
        [ div
            [ css
                [ flexGrow1
                , Css.fontSize Css.large
                , bold
                , lh 1.5
                , Px.p2 8 8
                ]
            ]
            [ text <| Project.title project ]
        , div [ css [ flex, selfCenter, Px.p2 0 8 ] ]
            [ IconButton.view Icon.Edit (editClicked projectId)
            , IconButton.view Icon.Comment noOp
            , IconButton.view Icon.PersonAdd noOp
            , IconButton.view Icon.MoreHorizontal noOp
            ]
        ]


viewInboxTitle : { noOp : msg } -> Html msg
viewInboxTitle { noOp } =
    div [ css [ flex, Px.pt 8 ] ]
        [ div
            [ css
                [ flexGrow1
                , Css.fontSize Css.large
                , bold
                , lh 1.5
                , Px.p2 8 8
                ]
            ]
            [ text inboxTitle ]
        , div [ css [ flex, selfCenter, Px.p2 0 8 ] ]
            [ IconButton.view Icon.Comment noOp
            , IconButton.view Icon.MoreHorizontal noOp
            ]
        ]
