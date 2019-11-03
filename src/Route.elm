module Route exposing (Route(..), filterHref, fromUrl, inboxHref, labelHref, projectHref, projectIdHref, replaceUrl)

import Browser.Navigation as Nav
import Filter exposing (Filter)
import FilterId exposing (FilterId)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attr
import Json.Decode as JD
import Json.Encode as JE
import Label exposing (Label)
import LabelId exposing (LabelId)
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser, map, parse, s, top)


type Route
    = Root
    | Inbox
    | Project ProjectId
    | Label LabelId
    | Filter FilterId
    | NotFound Url


parser : Parser (Route -> c) c
parser =
    Url.Parser.oneOf
        [ map Root top
        , map Inbox (s "inbox")
        , map Project (s "project" </> parseProjectId)
        , map Label (s "label" </> parseLabelId)
        , map Filter (s "filter" </> parseFilterId)
        ]


fromUrl : Url -> Route
fromUrl url =
    parse parser url
        |> Maybe.withDefault (NotFound url)


parseProjectId : Parser (ProjectId -> b) b
parseProjectId =
    Url.Parser.custom "PROJECT_ID" <|
        (JE.string
            >> JD.decodeValue ProjectId.decoder
            >> Result.toMaybe
        )


parseLabelId : Parser (LabelId -> b) b
parseLabelId =
    Url.Parser.custom "LABEL_ID" <|
        (JE.string
            >> JD.decodeValue LabelId.decoder
            >> Result.toMaybe
        )


parseFilterId : Parser (FilterId -> b) b
parseFilterId =
    Url.Parser.custom "FILTER_ID" <|
        (JE.string
            >> JD.decodeValue FilterId.decoder
            >> Result.toMaybe
        )


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToUrlString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToUrlString route)


routeToUrlString : Route -> String
routeToUrlString route =
    let
        pathSegments =
            case route of
                Root ->
                    []

                Inbox ->
                    [ "inbox" ]

                Project projectId ->
                    [ "project", ProjectId.toString projectId ]

                Label labelId ->
                    [ "label", LabelId.toString labelId ]

                Filter filterId ->
                    [ "filter", FilterId.toString filterId ]

                NotFound _ ->
                    []
    in
    Url.Builder.absolute pathSegments []


inboxHref : Attribute msg
inboxHref =
    href Inbox


projectIdHref : ProjectId -> Attribute msg
projectIdHref =
    Project >> href


projectHref : Project -> Attribute msg
projectHref =
    Project.id >> projectIdHref


labelHref : Label -> Attribute msg
labelHref =
    Label.id >> Label >> href


filterHref : Filter -> Attribute msg
filterHref =
    Filter.id >> Filter >> href
