module Fish where

import Color exposing (..)
import Graphics.Collage as C

import Physics
import Point

type alias Fish = Physics.Object {}

render : Fish -> C.Form
render {pos,vel} =
  let
    body = C.oval 80 40
    tail = C.polygon [(-40,-25), (-40,25), (-20,0)]
    forms = List.map (C.filled green) [body, tail]
    group = C.group forms
  in
     group |> C.move (Point.toPair pos)
