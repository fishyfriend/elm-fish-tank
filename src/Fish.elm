module Fish where


import Color exposing (..)
import Graphics.Collage exposing (..)

import Physics
import Point


type alias Fish = Physics.Object {}


render : Fish -> Form
render {pos}
  = oval 40 20
  |> filled green
  |> move (Point.toPair pos)
