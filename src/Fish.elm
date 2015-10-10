module Fish where


import Color exposing (..)
import Graphics.Collage exposing (..)


type alias Fish =
  { x : Int
  , y : Int
  }


render : Fish -> Form
render {x,y}
  = oval 40 20
  |> filled green
  |> move (toFloat x, toFloat y)
