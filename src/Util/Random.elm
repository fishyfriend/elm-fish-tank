module Util.Random where


import Random as R


map : (a -> b) -> R.Generator a -> R.Generator b
map f g =
  R.customGenerator <| \s ->
  R.generate g s |> \(r, s') ->
  (f r, s')

pointInRect : Float -> Float -> R.Generator (Float, Float)
pointInRect w h =
  let
    (xMax, yMax) = (w/2, h/2)
    (xMin, yMin) = (-xMax, -yMax)
  in
    R.pair (R.float xMin xMax) (R.float yMin yMax)

pointInCirc : Float -> R.Generator (Float, Float)
pointInCirc r =
  let
    rG = R.float 0 r
    aG = R.float 0 (turns 1)
    pG = R.pair rG aG
  in
    map fromPolar pG
