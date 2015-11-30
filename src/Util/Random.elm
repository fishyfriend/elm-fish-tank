module Util.Random where

import Random as R
import Point exposing (Point)

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
    R.map Point.fromPair p

pointInCirc : Float -> R.Generator (Point Float)
pointInCirc r =
  let
    rG = R.float 0 r
    θG = R.float 0 (turns 1)
    pG = R.pair rG θG
  in
    R.map (fromPolar >> Point.fromPair) pG
