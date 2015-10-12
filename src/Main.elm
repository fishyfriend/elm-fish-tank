import Char
import Color exposing (..)
import Debug exposing (log)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element, centered)
import List
import Keyboard exposing (KeyCode)
import Random exposing (initialSeed)
import Text
import Time exposing (..)
import Window

import Fish exposing (Fish)
import Point exposing (Point)
import Tank exposing (Tank)


-- MODEL --

type State
  = PreInit
  | Running { mode : Mode, tank: Tank }

type Mode = Watch | Tap

type alias Input =
  { time : Time
  , fps : Time
  , winSize : (Int, Int)
  , tKey : Bool
  , wKey : Bool }


-- UPDATE --

update : Input -> State -> State
update ({time, fps, winSize, tKey, wKey} as i) s =
  case s of
    PreInit ->
      let
        dims = winSize |> Point.fromPair |> Point.toFloat
        seed = time |> inMilliseconds >> round >> initialSeed
        tank = Tank.init 20 dims seed
      in
        Running { mode = Watch, tank = tank }
    Running ({mode,tank} as r) ->
      let
        m =
          case (mode, tKey, wKey) of
            (Tap, _, True) -> Watch
            (Watch, True, _) -> Tap
            _ -> mode
        t = Tank.update fps tank
      in
        Running { r | mode <- m, tank <- t }


-- VIEW --

view : State -> Point Float -> Element
view s {x,y} =
  case s of
    PreInit ->
      Text.fromString "Loading..." |> centered
    Running {mode,tank} ->
      let
        tankForm = Tank.render tank
        textY = -(min x tank.dims.x) / 2 + 20
        textForm = drawText mode |> moveY textY
      in
        collage (round x) (round y) [tankForm, textForm]

drawText : Mode -> Form
drawText mode =
  let
    msg =
      if | mode == Tap -> "Click to tap the tank!"
         | otherwise -> ""
  in
    (formatText >> toForm) msg

formatText : String -> Element
formatText = Text.fromString
  >> Text.color black
  >> Text.height 30
  >> centered


-- SIGNALS --

main : Signal Element
main =
  let
    dims = Signal.map (Point.toFloat << Point.fromPair) Window.dimensions
  in
    Signal.map2 view state dims

state : Signal State
state = Signal.foldp update PreInit input

input : Signal Input
input =
  let
    time = Signal.map fst timeAndFps
    fps = Signal.map snd timeAndFps
    winSize = Window.dimensions
    t = keyDown 'T'
    w = keyDown 'W'
  in
    Signal.map5 Input time fps winSize t w

timeAndFps : Signal (Time, Time)
timeAndFps = timestamp (fps 30)

keyDown : Char -> Signal Bool
keyDown k =
  k |> Char.toUpper >> Char.toCode >> Keyboard.isDown
