module Px exposing (..)

import Css exposing (margin, margin2, padding2, px)


ma =
    margin << px


m2 tb lr =
    margin2 (px tb) (px lr)


p2 tb lr =
    padding2 (px tb) (px lr)