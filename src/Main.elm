import Char
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element, centered)
import List
import Keyboard exposing (KeyCode)
import Text
import Window

import Fish exposing (Fish)
import Tank exposing (Tank)


-- MODEL --

type alias Model =
  { mode : Mode
  , tank : Tank
  }

type Mode = Tap | Watch

type alias Input =
  { t : Bool
  , w : Bool
  }

initialState : Model
initialState =
  { mode = Watch
  , tank =
      { width = 1000
      , height = 1000
      , fishes = [ Fish 10 20, Fish -30 -40, Fish 50 60 ]
      }
  }


-- UPDATE --

update : Input -> Model -> Model
update {t,w} ({mode,tank} as model) =
  { model |
      mode <-
        case (mode, t, w) of
          (Tap, _, True) -> Watch
          (Watch, True, _) -> Tap
          _ -> mode
  }


-- VIEW --

view : Model -> Int -> Int -> Element
view {mode,tank} winW winH =
  let
    tankForm = Tank.render tank
    textOffset = toFloat (20 - (min winH tank.height) // 2)
    textForm = drawText mode |> moveY textOffset
  in
    collage winW winH [tankForm, textForm]

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
main = Signal.map3 view state Window.width Window.height

state : Signal Model
state = Signal.foldp update initialState input

input : Signal Input
input =
  let
    t = keyDown 'T'
    w = keyDown 'W'
  in
    Signal.map2 Input t w

keyDown : Char -> Signal Bool
keyDown k =
  k |> Char.toUpper >> Char.toCode >> Keyboard.isDown
