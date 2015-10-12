module Tank where


import Color exposing (blue)
import Debug exposing (log)
import Graphics.Collage exposing (..)
import Random as R
import Time exposing (Time)

import Fish exposing (Fish)
import Physics
import Point exposing (Point)
import Util.Random as UR
import Wave exposing (Wave)


type alias Tank =
  { dims : Point Float
  , wave : Wave
  , fishes : List Fish }


init : Int -> Point Float -> Wave -> R.Seed -> Tank
init nFishes dims wave seed =
  let
    g = R.list nFishes (randomFish dims 0)
    (fs, _) = R.generate g seed
  in
     Tank dims wave fs

randomFish : Point Float -> Float -> R.Generator Fish
randomFish xyMax vMax =
  let
    pV = R.pair (UR.pointInRect xyMax) (UR.pointInCirc vMax)
  in
    UR.map (\(p,v) -> {pos=p,vel=v}) pV

update : Time -> Time -> Tank -> Tank
update t dt ({fishes,wave} as tank) =
  let
    fs = fishes
      |> List.map (Physics.update dt)
      |> List.map (Wave.apply wave t)
  in
     { tank | fishes <- fs }

render : Tank -> Form
render {dims,fishes} =
  let
    bg = rect dims.x dims.y
      |> filled blue
    fg = List.map Fish.render fishes
  in
     group (bg :: fg)
