module Physics where


import Time exposing (Time)

type alias Object =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float }

drag = 0.0

update : Float -> Object -> Object
update t ({x,y,vx,vy} as object) =
  { object
  | x <- x + vx * t
  , y <- y + vy * t
  , vx <- decel (drag * t) vx
  , vy <- decel (drag * t) vy
  }

decel : Float -> Float -> Float
decel d v =
  if v > 0 then
    max 0 (v - d)
  else
    min 0 (v + d)
