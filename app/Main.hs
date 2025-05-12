module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Orbit

-- Adjustable constants
window :: Display
window = InWindow "Planet Orbit Simulation" (800, 800) (100, 100)

background :: Color
background = black

fps :: Int
fps = 60

scaleFactor :: Float 
scaleFactor = 1e4

 

-- Main simulation loop
main :: IO ()
main = do
  -- preload initial images
  planetPic <- handlePNG (planetImage earth)
  satPic <- handlePNG "resources/sat.png"
  -- set initial start to simulation -> start with earth at 2000 km , 200 km/s, and 0 eccentricity
  let initModel = initialSimState earth 2000 200 0 planetPic satPic
  playIO window background fps initModel (return . makeSim scaleFactor) handleEvent (\dt state -> return (updateSimTime dt state))

-- handle user input 
handleEvent :: Event -> SimState -> IO SimState
handleEvent (EventKey (Char c) Down _ _) sim@(MkSimState p alt vel e pImage sImage t z)
  | c `elem` ['0'..'8'] = changePlanet (read [c]) sim
  | c `elem` ['q', 'a', 'w', 's', 'e', 'd'] = changeParameter c sim
handleEvent (EventKey (MouseButton WheelUp) Down _ _) (MkSimState p alt vel e pImage sImage t z) = 
  return (MkSimState p alt vel e pImage sImage t (min 5.0 (z * 1.1)))  -- Zoom in (limit to 5x)
handleEvent (EventKey (MouseButton WheelDown) Down _ _) (MkSimState p alt vel e pImage sImage t z) =
  return (MkSimState p alt vel e pImage sImage t (max 0.01 (z * 0.9))) -- Zoom out (limit to 0.2x)
handleEvent _ m = return m

-- previous main runs 

{-
main :: IO ()
main = display (InWindow "Planet Buttons" (800, 600) (100, 100)) black (
  displayButtons planetButtons volumeButtons)

  -- animateOrbit earth 2000 200  0 -- alt == km ; vel == km/s
  --animateOrbit mars 2000 200  0 -- alt == km ; vel == km/s
  -- animateOrbit moon 2000 600  0 -- alt == km ; vel == km/s

  {-

  if planet clicked do some sort of switch case to reinnitiate sim

  let planet = case planetChoice of
                 1 -> mercury
                 2 -> venus
                 3 -> earth
                 4 -> mars
                 5 -> jupiter
                 6 -> saturn
                 7 -> uranus 
                 8 -> neptune
                 0 -> moon
  animateOrbit planet altitude speed
  -}
  -}