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


type alias Tank =
  { dims : Point Float
  , fishes : List Fish }


init : Int -> Point Float -> R.Seed -> Tank
init n dims s =
  let
    g = R.list n (randomFish dims 10)
    (fs, _) = R.generate g s
  in
     Tank dims fs

randomFish : Point Float -> Float -> R.Generator Fish
randomFish xyMax vMax =
  let
    pV = R.pair (UR.pointInRect xyMax) (UR.pointInCirc vMax)
  in
    UR.map (\(p,v) -> {pos=p,vel=v}) pV

empty : Point Float -> Tank
empty dims = Tank dims []

update : Time -> Tank -> Tank
update t tank =
  let
    fs = List.map (Physics.update t) tank.fishes
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
