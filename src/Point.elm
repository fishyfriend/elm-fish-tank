module Point where

type alias Point a =
  { x : a
  , y : a
  }

fromPair : (a, a) -> Point a
fromPair (x, y) = { x = x, y = y }

toPair : Point a -> (a, a)
toPair p = (p.x, p.y)

convert : (a -> b) -> Point a -> Point b
convert f a = Point (f a.x) (f a.y)

toInt : Point Float -> Point Int
toInt p = convert round p

toFloat : Point Int -> Point Float
toFloat p = convert Basics.toFloat p
