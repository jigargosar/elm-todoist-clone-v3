module Px exposing (..)

import Css exposing (margin, margin2, padding2, paddingLeft, paddingRight, px)


ma =
    margin << px


m2 tb lr =
    margin2 (px tb) (px lr)


p2 tb lr =
    padding2 (px tb) (px lr)


pl =
    paddingLeft << px


pr =
    paddingRight << px
