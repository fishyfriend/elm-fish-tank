module Physics where


import Time exposing (Time)
import Point exposing (Point)


type alias Object a =
  { a
  | pos : Point Float
  , vel : Point Float }


drag = 0.1

update : Time -> Object a -> Object a
update time ({pos,vel} as a) =
  let
    t = Time.inSeconds time
    x' = pos.x + t * vel.x
    y' = pos.y + t * vel.y
    v'x = decel (t * drag) vel.x
    v'y = decel (t * drag) vel.y
  in
    { a
    | pos <- Point.fromPair (x', y')
    , vel <- Point.fromPair (v'x, v'y) }

decel : Float -> Float -> Float
decel d v =
  if v > 0 then
    max 0 (v - d)
  else
    min 0 (v + d)
