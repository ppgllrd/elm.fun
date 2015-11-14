----------------------------------------------------------------------------------
-- Simple animated Kaleido
--
-- Can be tested online on http://elm-lang.org/try
--
-- Pepe Gallardo, 2015
----------------------------------------------------------------------------------

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (Time)


dimX = 600
dimY = 600

triangle : Float -> Form
triangle alpha = filled red <| ngon 3 50

time : Signal Time
time = Signal.map Time.inSeconds (Time.every (25 * Time.millisecond))

slowTime : Float -> Signal Time
slowTime f = Signal.map (\t -> t/f) time


main = Signal.map (\fs -> collage dimX dimY 
           ( (rect dimX dimY |> filled black) :: fs)) (kaleido 6)

colors : List Color
colors = [ yellow, orange, red,  purple, blue, green ]

kaleido : Int -> Signal (List Form)
kaleido n = 
 let
   rads = List.map (\x -> 2*pi*toFloat x/toFloat n) [0..n-1]
   coords = combine (List.map star rads)
   p col = Signal.map (polyg col) coords
   polys = combine (List.map2 (\alpha col -> Signal.map (rotate alpha) (p col)) rads colors)
 in Signal.map2 (\ps t -> List.map (rotate (2*pi*sin t)) ps) 
            polys
            (slowTime 3)
 

type alias Coord = (Float,Float)

star : Float -> Signal Coord
star x = 
 let
   sl = slowTime 50000
   l = Signal.map (\t -> x*(t + 1)) sl
   s = Signal.map sin l
   c = Signal.map cos l
   cx = Signal.map2 (\c l -> 2 * cos (x*c + l)) c l
   cy = Signal.map3 (\sl s l -> 2 * abs (sin (sl*s - l))) sl s l
 in Signal.map2 (,) cx cy
 
 
polyg : Color -> List Coord -> Form 
polyg col cs =  scale 75 << filled col <| polygon cs 
 
combine : List (Signal a) -> Signal (List a)
combine = List.foldr (Signal.map2 (::)) (Signal.constant [])


{- 

deg t = degrees (toFloat (round t % 360))

showT : Time -> Element
showT t = collage (2*dimX) (2*dimY) [ rotate (deg (6*t)) <| triangle t
                                    , text (fromString (toString t))
                                    ]

-}
