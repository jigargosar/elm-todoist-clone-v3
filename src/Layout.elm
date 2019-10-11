module Layout exposing (Layout, Msg, Parts, initial, openDrawer, update, view)

import Css
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events exposing (onClick)
import Lens
import Styles exposing (..)


type Layout
    = Layout Private


initial : Layout
initial =
    Layout <| Private False


type alias Private =
    { drawerModal : Bool }


type Msg
    = OpenModalDrawer
    | CloseModalDrawer


openDrawer : Msg
openDrawer =
    OpenModalDrawer


privateLens : { get : Private -> small, set : small -> Private -> Private } -> Lens.System small Layout
privateLens =
    Lens.system >> Lens.compose (Lens.system { get = \(Layout p) -> p, set = \s _ -> Layout s })


drawerModalL : Lens.System Bool Layout
drawerModalL =
    privateLens { get = .drawerModal, set = \s b -> { b | drawerModal = s } }


update : (Msg -> msg) -> Msg -> Layout -> ( Layout, Cmd msg )
update toMsg message model =
    case message of
        OpenModalDrawer ->
            ( drawerModalL.set True model, Cmd.none )

        CloseModalDrawer ->
            ( drawerModalL.set False model, Cmd.none )


type alias Parts msg =
    { appbar : List (Html msg)
    , drawer : List (Html msg)
    , content : List (Html msg)
    }


view : (Msg -> msg) -> Parts msg -> Layout -> Html msg
view toMsg { appbar, drawer, content } layout =
    styled div
        [ bgBody, h_100 ]
        []
        [ styledAppbar [] appbar
        , styled div
            [ center, w_100, max_w_app, h_100 ]
            []
            [ styledPermanentDrawer []
                -- TEST OVERFLOW SCROLL
                -- [ styled div [ Css.height (Css.vh 200) ] [] drawer ]
                drawer
            , styledMain [] content
            ]
        , viewModalDrawer toMsg
            layout
            -- TEST OVERFLOW SCROLL
            -- [ styled div [ Css.height (Css.vh 200) ] [] drawer ]
            drawer
        ]


styledAppbar appbarAttrs appbarContent =
    styled div
        [ fgWhite
        , bgLightRed
        , batch [ fixed, top_0, w_100, h_header ]
        , flex
        , z_ 10
        , Css.property "box-shadow" "0 1px 4px rgba(0,0,0,0.3)"
        ]
        []
        [ styled header
            [ batch [ center, w_100, max_w_app ]
            , batch [ ph 2, flex, itemsCenter ]
            ]
            appbarAttrs
            appbarContent
        ]


styledMain contentAttrs content =
    styled div
        [ batch [ ns [ ml_ sidebarWidthPx ], transition [ Transitions.marginLeft 150 ] ]
        , pt_ headerHeightPx
        , h_100
        , bgWhite
        , ns [ br_, bl, b__main ]
        , flex
        ]
        []
        [ styled main_ [ flexGrow1 ] contentAttrs content ]


styledPermanentDrawer =
    styled aside
        [ batch [ slideOutDrawer, ns [ slideInDrawer ], transition [ Transitions.transform 150 ] ]
        , batch [ fixed, top_0, bottom_0, pt_ headerHeightPx, w_sidebar ]
        , autoHideScrollY
        ]


viewModalDrawer toMsg layout drawer =
    let
        drawerModalOpen =
            drawerModalL.get layout
    in
    styled div
        [ z_ 10
        , batch [ fixed ]
        ]
        []
        [ styled div
            [ batch [ fixed, absFill ]
            , bg (Css.hsla 0 0 0 0.3)
            , styleIf (not drawerModalOpen) [ dn ]
            , ns [ dn ]
            ]
            [ onClick <| toMsg CloseModalDrawer ]
            []
        , styled aside
            [ batch [ fixed, top_0, bottom_0, w_sidebar, max_w_pct 90 ]
            , bgWhite
            , transition [ Transitions.transform 150, Transitions.visibility 150 ]
            , batch <|
                if drawerModalOpen then
                    [ visible, slideInDrawer ]

                else
                    [ hidden, slideOutDrawer ]
            , ns [ hidden, slideOutDrawer ]
            , autoHideScrollY
            ]
            [ class "shadow-1" ]
            drawer
        ]



-- Custom Styles


bgBody : Style
bgBody =
    bgGrayN 0.98


fgWhite =
    fg white


b__main =
    b__ (grayN 0.95)


headerHeightPx =
    50


maxAppWidthPx =
    922


sidebarWidthPx =
    266


h_header =
    h_ headerHeightPx


w_sidebar =
    w_ sidebarWidthPx


max_w_app =
    max_w maxAppWidthPx


slideOutDrawer =
    Css.transforms [ Css.translateX <| Css.px -sidebarWidthPx ]


slideInDrawer =
    Css.transforms [ Css.translateX <| Css.px 0 ]
