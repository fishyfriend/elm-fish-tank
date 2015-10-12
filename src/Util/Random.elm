module Util.Random where


import Random as R

import Point exposing (Point)

map : (a -> b) -> R.Generator a -> R.Generator b
map f g =
  R.customGenerator <| \s ->
  R.generate g s |> \(r, s') ->
  (f r, s')

pointInRect : Point Float -> R.Generator (Point Float)
pointInRect dims =
  let
    (w, h) = Point.toPair dims
    (xMax, yMax) = (w/2, h/2)
    (xMin, yMin) = (-xMax, -yMax)
    x = R.float xMin xMax
    y = R.float yMin yMax
    p = R.pair x y
  in
    map Point.fromPair p

pointInCirc : Float -> R.Generator (Point Float)
pointInCirc r =
  let
    rG = R.float 0 r
    θG = R.float 0 (turns 1)
    pG = R.pair rG θG
  in
    map (fromPolar >> Point.fromPair) pG
