module Tank where


import Color exposing (blue)
import Debug exposing (log)
import Graphics.Collage exposing (..)
import Random as R
import Time exposing (Time)

import Fish exposing (Fish)
import Physics
import Util.Random as UR


type alias Tank =
  { width : Float
  , height : Float
  , fishes : List Fish }

init : Int -> (Float, Float) -> R.Seed -> Tank
init n (w, h) s =
  let
    g = R.list n (randomFish (w, h) 10)
    (fs, _) = R.generate g s
  in
     Tank w h fs

randomFish : (Float, Float) -> Float -> R.Generator Fish
randomFish (w, h) vMax =
  let
    p = R.pair (UR.pointInRect w h) (UR.pointInCirc vMax)
  in
    (flip UR.map) p <| \((x, y), (vx, vy)) ->
    Fish x y vx vy

empty : (Float, Float) -> Tank
empty (w, h) = Tank w h []

update : Float -> Tank -> Tank
update t ({fishes} as tank) =
  { tank |
    fishes <- List.map (Physics.update t) fishes }

render : Tank -> Form
render {width,height,fishes} =
  let
    bg = rect width height
      |> filled blue
    fg = List.map Fish.render fishes
  in
     group (bg :: fg)
