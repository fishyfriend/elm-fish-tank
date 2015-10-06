import Char
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element, centered)
import Keyboard exposing (KeyCode)
import Text
import Window


-- Model --

type alias Model =
  { mode : Mode
  , tank : Tank
  }

type Mode = Tap | Watch

type alias Tank =
  { width : Int
  , height : Int
  , fish : List (Int, Int)
  }

type alias Input =
  { t : Bool
  , w : Bool
  }

initialState : Model
initialState =
  { mode = Watch
  , tank =
      { width = 400
      , height = 200
      , fish = [ (10, 20), (-30, -40), (50, 60) ]
      }
  }


-- Update --

update : Input -> Model -> Model
update {t,w} ({mode,tank} as model) =
  { model |
      mode <-
        case (mode, t, w) of
          (Tap, _, True) -> Watch
          (Watch, True, _) -> Tap
          _ -> mode
  }


-- View --

view : Model -> (Int, Int) -> Element
view {mode,tank} (w,h) =
  let
    bkgd = rect (toFloat w) (toFloat h) |> filled blue
    fish = List.map (\(x, y) -> drawFish x y) tank.fish
    textM = drawText mode
    forms = textM
      |> Maybe.map (\t -> bkgd :: fish ++ [t])
      |> Maybe.withDefault (bkgd :: fish)
  in
    collage w h forms

drawFish : Int -> Int -> Form
drawFish x y
  = oval 40 20
  |> filled green
  |> move (toFloat x, toFloat y)

drawText : Mode -> Maybe Form
drawText mode =
  let
    msg =
      if mode == Tap then Just "Click to tap the tank!"
      else Nothing
  in
    Maybe.map formatText msg

formatText : String -> Form
formatText = Text.fromString
  >> Text.color white
  >> Text.height 30
  >> centered
  >> toForm


-- Signals --

main = Signal.map2 view state Window.dimensions

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
