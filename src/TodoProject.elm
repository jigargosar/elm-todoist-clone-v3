module TodoProject exposing
    ( Model
    , fromProject
    , inbox
    , view
    , viewInboxTitle
    , viewProjectTitle
    )

import CColor exposing (CColor)
import Color exposing (Color)
import Css
import Html.Styled exposing (Attribute, Html, a, div, text)
import Html.Styled.Attributes exposing (css)
import InboxOrProject exposing (InboxOrProject)
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Px
import Route
import Styles exposing (..)
import UI.Icon as Icon
import UI.IconButton as IconButton


type Model
    = Model Internal


type alias Internal =
    InboxOrProject () Project


inbox : Model
inbox =
    InboxOrProject.inbox ()
        |> Model


fromProject : Project -> Model
fromProject =
    InboxOrProject.project >> Model


inboxTitle : String
inboxTitle =
    "Inbox"


inboxColor : Color
inboxColor =
    CColor.toColor CColor.Charcoal


inboxHref : Attribute msg
inboxHref =
    Route.inboxHref


unwrap : Model -> Internal
unwrap (Model internal) =
    internal


title : Model -> String
title =
    unwrap >> InboxOrProject.unwrap inboxTitle Project.title


color_ : Model -> Color
color_ =
    unwrap >> InboxOrProject.unwrap inboxColor (Project.cColor >> CColor.toColor)


cssColor : Model -> Css.Color
cssColor =
    color_ >> toCssColor


highContrastCssColor : Model -> Css.Color
highContrastCssColor =
    color_ >> Color.highContrast >> toCssColor


href : Model -> Attribute msg
href =
    unwrap >> InboxOrProject.unwrap inboxHref Route.projectHref


view : Model -> Html msg
view model =
    a
        [ css
            [ linkReset
            , ph 1
            , lh 1.5
            , Css.fontSize Css.small
            , bg (cssColor model)
            , fg (highContrastCssColor model)
            , boRad 2
            , hover [ underline, pointer ]
            ]
        , href model
        ]
        [ text <| title model ]


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
