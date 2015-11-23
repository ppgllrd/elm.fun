----------------------------------------------------------------------------------
-- A snake of hexagons follows mouse
--
-- Can be tested online on http://elm-lang.org/try
--
-- Pepe Gallardo, 2015
----------------------------------------------------------------------------------

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Time
import Signal


hexagon : Color -> Form
hexagon clr =
  filled clr (ngon 6 10)


main : Signal Element
main =
  Signal.map showL signals


signals : Signal (List (Int,Int))
signals =
  combine (List.map (flip Time.delay Mouse.position) <| List.map (\x -> x*75) [0..19])


dimX = 400
dimY = 400


toScreen : (Int,Int) -> (Float,Float)
toScreen (x,y) =
  (-dimX + toFloat x, dimY - toFloat y)


toColor : Int -> List a -> Color
toColor i xs =
 let
   minRed = 175
   l = List.length xs
 in
   rgb (minRed + (l-i)*(255-minRed) // l) 0 0


showL : List (Int,Int) -> Element
showL xs =
  collage (2*dimX) (2*dimY) <|
  List.indexedMap (\i pt -> move (toScreen pt) <|  hexagon (toColor i xs)) xs


combine : List (Signal a) -> Signal (List a)
combine =
  List.foldr (Signal.map2 (::)) (Signal.constant [])
