module Orbit where

import Graphics.Gloss
import Equations
import Graphics.Gloss.Juicy
import Codec.Picture
import Paths_sat_orbit_sim (getDataFileName)
import System.FilePath ((</>))
import Brillo (Color)

type Altitude = Float -- in km
type Velocity = Float -- in km/s
type Eccentricity = Float 

-- Planet data to hold necessary values 
data Planet = 
    MkPlanet {
        planetId :: Int ,  -- corresponds with closeness to sun
        planetMass :: Float , -- in kg
        planetRadius :: Float ,  -- in km
        planetImage :: FilePath,
        planetColor :: Color -- file name of png
    } 


-- handles the planet image file path and converts to gloss picture
-- https://hackage.haskell.org/package/JuicyPixels-3.3.9/docs/Codec-Picture.html
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Either.html

handlePNG :: FilePath -> IO Picture
handlePNG fileName = do
  resultImg <- readImage fileName -- readImage :: FilePath -> IO (Either String DynamicImage)
  go resultImg 
    where
        go (Left err) = error ("Image load error: " ++ err)
        go (Right img) = return pic
            where 
                Just pic = fromDynamicImage img -- making assumptiong it will just be an img 


-- need to calculate the radius of the image then convert it to the planet radius for scaling
scalePlanetImage :: Float -> Planet -> Picture -> Picture 
scalePlanetImage scaleFactor planet planetImg = scale (1/10) (1/10) planetImg
   -- where 
       -- sf = (kmToMeters(planetRadius planet)) / scaleFactor



animateOrbit :: Planet -> Altitude -> Velocity -> Eccentricity -> IO ()
animateOrbit planet alt vel ecc = do
    planetPNG <- handlePNG (planetImage planet)
    satPNG <- handlePNG "resources/sat.png"
    let
      scaleFactor = 1e5 -- scale to screen --> may change later

      -- convert to meter units
      radiO_meters = kmToMeters(planetRadius planet) + kmToMeters(alt) 
      v = kmToMeters vel
      
      -- get period and angular frequency
      period = orbitPeriod radiO_meters v
      w = 2 * pi / period

      -- set semi-minor axis 
      b = semiMinorAxis radiO_meters ecc


      -- make display pane 
      display = InWindow "Orbit Simulation" (800, 800) (100, 100)

      satellitePos t =  (realToFrac x / scaleFactor, realToFrac y / scaleFactor)
        where
            t' = realToFrac t :: Float
            x = radiO_meters * cos (t' * w)
            y = b * sin (t' * w)

        
      render t = pictures [ --color blue (translate 0 0 (circleSolid ((kmToMeters(planetRadius planet)) / scaleFactor)))  -- planet
                            translate 0 0 (scalePlanetImage scaleFactor planet planetPNG)
                          , color white (scale 1 (b / radiO_meters) (circle (realToFrac (radiO_meters / scaleFactor))))
                          , translate x y (scale (1/100) (1/100) satPNG)]
        where 
            (x, y) = satellitePos t

    animate display black render