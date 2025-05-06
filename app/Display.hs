module Display where

import Brillo.Data.Picture
import Graphics.Gloss.Juicy
import Equations
import Orbit

createPlanetOptions :: UI -> [Planet] -> IO [Picture]
createPlanetOptions _ [] = []
createPlanetOptions ui (p:ps) = do
    image <- handlePNG (planetImage p)
    others <- createPlanetOptions ui ps
    image : others

