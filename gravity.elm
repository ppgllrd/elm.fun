----------------------------------------------------------------------------------
-- Simulator of particles
--
-- Can be tested online on http://elm-lang.org/try
--
-- Pepe Gallardo, 2015
----------------------------------------------------------------------------------

import Color exposing (black, green)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (map, map2, foldr)
import Time
import Window

----------------------------------------------------------------------------------
-- Model
----------------------------------------------------------------------------------

type alias Vector =
  { x : Float
  , y : Float
  }


add : Vector -> Vector -> Vector
add v1 v2 =
  { x = v1.x + v2.x
  , y = v1.y + v2.y
  }


diff : Vector -> Vector -> Vector
diff v1 v2 =
  { x = v1.x - v2.x
  , y = v1.y - v2.y
  }


times : Float -> Vector -> Vector
times n v =
  { x = n * v.x
  , y = n * v.y
  }


modulus : Vector -> Float
modulus v =
  sqrt (v.x^2 + v.y^2)


type alias Particle =
  { pos : Vector -- position
  , v : Vector   -- velocity
  , m : Float    -- mass
  , r : Float    -- radius
  }


applyForce : Particle -> Vector -> Float -> Particle
applyForce p f dt =
  let
    a = (1/p.m) `times` f
    v' = p.v `add` (dt `times` a)
  in { p |
       v = v'
     , pos = p.pos `add` (dt `times` v')
     }


force : Particle -> Particle -> Vector
force p1 p2 =
  let
    g = 6.67e-11 -- gravitational constant
    d = p2.pos `diff` p1.pos
    dist = modulus d
    f = g * p1.m * p2.m / dist^2
  in
    (f/dist) `times` d


type alias Universe = List Particle


advance : Universe -> Float -> Universe
advance us dt =
  let
    totalForce (p,ps) = foldr add { x=0, y=0 } (map (force p) ps)
    fs = map totalForce (split us)
  in
    map2 (\p f -> applyForce p f dt) us fs


split : List a -> List (a, List a)
split xs =
  case xs of
    []    -> []
    x::xs -> (x,xs) :: map (\(y,ys) -> (y,x::ys)) (split xs)


----------------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------------

view : (Int, Int) -> Universe -> Element
view (w',h') us =
  let
    (w,h) = (toFloat w', toFloat h')
  in
    collage w' h' <|
      map draw us


factor : Float
factor = 4e8


draw : Particle -> Form
draw p =
  let
    c = circle (p.r/factor)
  in
    group [ c |> outlined { defaultLine | width = 3 }
          , c |> filled green
          ]
      |> move (p.pos.x/factor, p.pos.y/factor)


----------------------------------------------------------------------------------
-- Examples
----------------------------------------------------------------------------------

type alias Simulation =
  { delta : Float
  , u : Universe
  }


sim1 : Simulation
sim1 =
  let
    p1 = { pos = { x = 0, y = 4.5e10 }
         , v = { x = 3e4, y = 0 }
         , m = 2.999e30
         , r = 5e9
         }

    p2 = { pos = { x = 0, y = -4.5e10 }
         , v = { x = -3e4, y = 0 }
         , m = 2.999e30
         , r = 5e9
         }

    p3 = { pos = { x = 0, y = 0 }
         , v = { x = 0.05e4, y = 0 }
         , m = 6.97e24
         , r = 2.5e9
         }
  in
    { delta = 1800
    , u = [ p1, p2, p3 ]
    }


sim2 : Simulation
sim2 =
  let
    p1 = { pos = { x = -3.5e10, y = 0 }
         , v = { x = 0, y = 1.4e3 }
         , m = 3e28
         , r = 5e9
         }

    p2 = { pos = { x = 3.5e10, y = 0 }
         , v = { x = 0, y = -1.4e3 }
         , m = 3e28
         , r = 5e9
         }

    p3 = { pos = { x = -1e10, y = 0 }
         , v = { x = 0, y = 1.5e4 }
         , m = 3e28
         , r = 5e9
         }

    p4 = { pos = { x = 1e10, y = 0 }
         , v = { x = 0, y = -1.5e4 }
         , m = 3e28
         , r = 5e9
         }
  in
    { delta = 20000
    , u = [ p1, p2, p3, p4 ]
    }


sim3 : Simulation
sim3 =
  let
    p1 = { pos = { x = -3.5e10, y = 0 }
         , v = { x = 0, y = 1.4e3 }
         , m = 3e28
         , r = 5e9
         }

    p2 = { pos = { x = 3.5e10, y = 0 }
         , v = { x = 0, y = -1.4e3 }
         , m = 3e28
         , r = 5e9
         }

    p3 = { pos = { x = -1e10, y = 0 }
         , v = { x = 0, y = 1.6e4 }
         , m = 3e28
         , r = 5e9
         }

    p4 = { pos = { x = 1e10, y = 0 }
         , v = { x = 0, y = -1.6e4 }
         , m = 3e28
         , r = 5e9
         }
  in
    { delta = 20000
    , u = [ p1, p2, p3, p4 ]
    }


----------------------------------------------------------------------------------
-- Program
----------------------------------------------------------------------------------

main : Signal Element
main =
  mainWith sim1


mainWith : Simulation -> Signal Element
mainWith sim =
  Signal.map2 view Window.dimensions
                   (Signal.foldp (\_ u -> repeat 7 (flip advance sim.delta) u) sim.u (Time.fps 30))

repeat : Int -> (a -> a) -> (a -> a)
repeat n f =
  case n of
    0 -> identity
    n -> f >> repeat (n-1) f
