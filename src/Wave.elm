module Wave where


import Time exposing (Time)

import Point exposing (Point)
import Physics exposing (Object)


type alias Wave =
  { freq : Float
  , amp : Float }


value : Wave -> Time -> Float
value {freq,amp} t =
  let
    secs = Time.inSeconds t
    scaled = secs * 2 * pi / freq
  in
    amp * sin scaled

apply : Wave -> Time -> Object a -> Object a
apply w t ({vel} as o) =
  let
    v'y = vel.y + value w t
    v' = Point.fromPair (vel.x, v'y)
  in
    { o | vel <- v' }
