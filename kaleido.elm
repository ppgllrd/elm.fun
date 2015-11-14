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
import Signal exposing ((<~),(~))
import List exposing (map, map2, foldr)


dimX = 600
dimY = 600

time : Signal Time
time = Time.inSeconds <~ Time.every (25 * Time.millisecond)

slowTime : Float -> Signal Time
slowTime f = (\t -> t/f) <~ time


main = (\f -> collage dimX dimY ((rect dimX dimY |> filled black) :: [f])) 
         <~ kaleido 6

colors : List Color
colors = [ yellow, orange, red,  purple, blue, green ]

kaleido : Int -> Signal Form
kaleido n = 
 let
   rads = map (\x -> 2*pi*toFloat x/toFloat n) [0..n-1]
   coords = combine (map star rads)
   p col = polyg col <~ coords
   polys = combine (map2 (\alpha col -> rotate alpha <~ p col) rads colors)
 in (\ps t -> group (map (rotate (2*pi*sin t)) ps)) 
            <~ polys ~ slowTime 3


type alias Coord = (Float,Float)

star : Float -> Signal Coord
star x = 
 let
   sl = slowTime 50000
   l = (\t -> x*(t + 1)) <~ sl
   s = sin <~ l
   c = cos <~ l
   cx = (\c l -> 2 * cos (x*c + l)) <~ c ~ l
   cy = (\sl s l -> 2 * abs (sin (sl*s - l))) <~ sl ~ s ~ l
 in (,) <~ cx ~ cy
  
polyg : Color -> List Coord -> Form 
polyg col cs = scale 75 << filled col <| polygon cs 
 
combine : List (Signal a) -> Signal (List a)
combine = foldr (Signal.map2 (::)) (Signal.constant [])


{- 
deg t = degrees (toFloat (round t % 360))
showT : Time -> Element
showT t = collage (2*dimX) (2*dimY) [ rotate (deg (6*t)) <| triangle t
                                    , text (fromString (toString t))
                                    ]
-}
