module TodoProject exposing
    ( TodoProject
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


type alias TodoProject =
    InboxOrProject Project


inboxTitle : String
inboxTitle =
    "Inbox"


inboxColor : Color
inboxColor =
    CColor.toColor CColor.Charcoal


inboxHref : Attribute msg
inboxHref =
    Route.inboxHref


unwrap : a -> (Project -> a) -> TodoProject -> a
unwrap =
    InboxOrProject.unwrap


inboxViewModel : ViewModel msg
inboxViewModel =
    { title = inboxTitle
    , href = inboxHref
    , color = inboxColor
    }


projectViewModel : Project -> ViewModel msg
projectViewModel p =
    { title = Project.title p
    , href = Route.projectHref p
    , color = Project.cColor p |> CColor.toColor
    }


type alias ViewModel msg =
    { title : String, href : Attribute msg, color : Color }


toViewModel : TodoProject -> ViewModel msg
toViewModel =
    unwrap
        inboxViewModel
        projectViewModel


view : TodoProject -> Html msg
view model_ =
    let
        vm : ViewModel msg
        vm =
            toViewModel model_
    in
    a
        [ css
            [ linkReset
            , ph 1
            , lh 1.5
            , Css.fontSize Css.small
            , bg (toCssColor vm.color)
            , fg (toCssColor <| Color.highContrast vm.color)
            , boRad 2
            , hover [ underline, pointer ]
            ]
        , vm.href
        ]
        [ text vm.title ]


viewProjectTitle :
    { editClicked : ProjectId -> msg, noOp : msg }
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
