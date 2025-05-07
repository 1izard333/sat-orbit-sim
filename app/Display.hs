module Display where

import Brillo
import Equations
import Orbit

createPlanetButton :: Planet -> Picture
createPlanetButton p = circleSolid

handleEvent :: Event -> [[Int]] -> [[Int]]
handleEvent event = case Event of 
    (EventKey (MouseButton LeftButton) _ _ (sx, sy)) -> 