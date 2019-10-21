module Layout exposing (Parts, view)

import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events exposing (onClick)
import Styles exposing (..)


type alias Parts msg =
    { appbar : List (Html msg)
    , drawer : { content : List (Html msg), portal : List (Html msg) }
    , main : { content : List (Html msg), portal : List (Html msg) }
    }


view : { closeDrawerModal : msg } -> Parts msg -> Bool -> Html msg
view { closeDrawerModal } { appbar, drawer, main } isDrawerModalOpen =
    styled div
        [ bgBody
        , c_grayL 0.3
        ]
        [ class "sans-serif" ]
        ([ styledAppbar [] appbar
         , styled div
            [ center
            , w_100
            , max_w_app
            ]
            []
            [ styledPermanentDrawer
                -- TEST OVERFLOW SCROLL
                -- [ styled div [ Css.height (Css.vh 200) ] [] drawer ]
                drawer.content
            , styledMain [] main.content
            ]
         , viewModalDrawer closeDrawerModal
            isDrawerModalOpen
            -- TEST OVERFLOW SCROLL
            -- [ styled div [ Css.height (Css.vh 200) ] [] drawer ]
            drawer.content
         ]
            ++ drawer.portal
            ++ main.portal
        )


styledAppbar appbarAttrs appbarContent =
    styled div
        [ fgWhite
        , bgLightRed
        , batch [ fixed, top_0, w_100, h_header ]
        , flex
        , z_ 10
        , Css.property "box-shadow" "0 1px 4px rgba(0,0,0,0.3)"
        , commonTransitions
        ]
        []
        [ styled header
            [ batch [ center, w_100, max_w_app ]
            , batch [ ph 2, ns [ ph 3 ], flex, itemsCenter ]
            ]
            appbarAttrs
            appbarContent
        ]


styledMain contentAttrs content =
    styled div
        [ commonTransitions
        , batch
            [ ns
                [ ml_ sidebarWidthPx
                , Css.maxWidth <| Css.calc (Css.vw 100) Css.minus (Css.px sidebarWidthPx)
                , Css.width <| Css.px mainWidthPx
                ]
            ]
        , pt_fix_for_header
        , h_100
        , bgWhite
        , ns [ bo_r, bo_l, b__main ]
        , flex
        ]
        []
        [ styled main_ [ flexGrow1, w_100 ] contentAttrs content ]


styledPermanentDrawer drawer =
    aside
        [ css
            [ commonTransitions
            , batch [ slideOutDrawer, ns [ slideInDrawer ] ]
            , batch [ fixed, top_0, pt_fix_for_header, w_sidebar, vh 100 ]
            , autoHideScrollY
            , Css.overflowX Css.hidden
            ]
        ]
        [ div [ css [ w_sidebar, pb 5 ] ] drawer ]


viewModalDrawer closeDrawerModal isDrawerModalOpen drawerContent =
    styled div
        [ z_ 10
        , batch [ fixed ]
        ]
        []
        [ styled div
            [ batch [ fixed, absFill ]
            , bg (Css.hsla 0 0 0 0.3)
            , styleIf (not isDrawerModalOpen) [ dn ]
            , ns [ dn ]
            ]
            [ onClick closeDrawerModal ]
            []
        , styled aside
            [ batch [ fixed, top_0, bottom_0, w_sidebar, max_vw 90 ]
            , bgWhite
            , commonTransitions
            , batch <|
                if isDrawerModalOpen then
                    [ visible, slideInDrawer ]

                else
                    [ hidden, slideOutDrawer ]
            , ns [ hidden, slideOutDrawer ]
            , autoHideScrollY
            , Css.overflowX Css.hidden
            ]
            [ class "shadow-1" ]
            [ div [ css [ w_sidebar, max_vw 90, pb 5 ] ] drawerContent ]
        ]



-- Custom Styles


bgBody : Style
bgBody =
    bgGrayL 0.98


fgWhite =
    fg white


b__main =
    boc (grayL 0.95)


headerHeightPx =
    36


ns_headerHeightPx =
    48


pt_fix_for_header =
    batch [ pt_ headerHeightPx, ns [ pt_ ns_headerHeightPx ] ]


h_header =
    batch [ h_ headerHeightPx, ns [ h_ ns_headerHeightPx ] ]


maxAppWidthPx =
    922


sidebarWidthPx =
    266


mainWidthPx =
    maxAppWidthPx - sidebarWidthPx


w_sidebar =
    w_ sidebarWidthPx


max_w_app =
    max_w maxAppWidthPx


slideOutDrawer =
    Css.transforms [ Css.translateX <| Css.px -sidebarWidthPx ]


slideInDrawer =
    Css.transforms [ Css.translateX <| Css.px 0 ]
