module Orbit where

import Graphics.Gloss
import Equations
import Graphics.Gloss.Juicy
import Codec.Picture
import Paths_sat_orbit_sim (getDataFileName)
import System.FilePath ((</>))
import Brillo (Color)
import Equations (orbitalPosX, kmToMeters)
import Data.List (group)

-- made types just for readability 
type Altitude = Float  -- in km
type Velocity = Float  -- in km/s
type Eccentricity = Float

-- image radius in px
imgRadius :: Float 
imgRadius = 268.5

-- Planet data with planet info
data Planet = MkPlanet
  { planetId     :: Int -- id for planet based on distance from sun (except moon) 
  , planetMass   :: Float -- mass of planet (kg)
  , planetRadius :: Float -- radius of planet (km)
  , radiusRatio  :: Float -- ratio of radius size to earth (used for scaling images)
  , planetImage  :: FilePath -- file path for the planet image
  } deriving (Eq, Show) -- used for logs 


-- Data to hold the current state of the simulation 
data SimState = MkSimState
  { currentPlanet :: Planet -- current planet being orbited 
  , altSim        :: Altitude -- current altitude of orbit 
  , velSim        :: Velocity -- current velocity of orbit 
  , eccentricity  :: Eccentricity -- current eccentricity of orbit
  , planetImg     :: Picture -- the planet image
  , satImg        :: Picture -- sat image
  , timeT         :: Float -- timeT of orbit for position 
  , zoom          :: Float
  }

-- maps data to its id for calling 
planetMap :: [(Int, Planet)]
planetMap = map go [ moon, mercury, venus, earth, mars, jupiter, saturn, uranus, neptune ]
    where
        go planet = (planetId planet, planet)


-- planets list ( https://nssdc.gsfc.nasa.gov/planetary/factsheet/planet_table_ratio.html ) -> data pulled from here

mercury :: Planet
mercury = MkPlanet 1 0.33010e24 2440.5 0.383 "resources/mercury.png"

venus :: Planet
venus = MkPlanet 2 4.8673e24 6051.8 0.949 "resources/venus.png"

earth :: Planet
earth = MkPlanet 3 5.9722e24 6378.137 1 "resources/earth.png"

mars :: Planet
mars = MkPlanet 4 0.64169e24 3396.2 0.532 "resources/mars.png"

jupiter :: Planet
jupiter = MkPlanet 5 1898.13e24 71492 11.21 "resources/jupiter.png"

saturn :: Planet
saturn = MkPlanet 6 568.32e24 60268 9.45 "resources/saturn.png"

uranus :: Planet
uranus = MkPlanet 7 86.811e24 25559 4.01 "resources/uranus.png"

neptune :: Planet
neptune = MkPlanet 8 102.409e24 24764 3.88 "resources/neptune.png"

moon :: Planet
moon = MkPlanet 0 0.07346e24 1738.1 0.2724 "resources/moon.png"


-- handle PNG image -> converts image file path to t angable IO picture for use on graphics
handlePNG :: FilePath -> IO Picture
handlePNG path = do
  -- Codec.Picture readImage :: FilePath -> IO (Either String DynamicImage) 
  img <- readImage path
  -- Either a b -- a = Left , b = Right 
  case img of
    -- a = Error message , b = DynamicImage 
    Left a -> error ("Image load error: " ++ a)
    -- fromDynamicImage :: DynamicImage -> Maybe(Image S cs e)
    Right b -> return (go (fromDynamicImage b))
        where 
            go Nothing = Blank -- return blank image
            go (Just picture) = picture



-- 
calculateScaleFactor :: Float -> Float -> Planet -> Float 
calculateScaleFactor width height planet = 
    -- We want the planet plus some padding to fit in the smaller window dimension
    (max (planetRadius planet * radiusRatio planet) minPlanetDisplaySize) * safetyMargin / minWindowDimension
  where 
    minWindowDimension = min width height
    -- Minimum display size so small planets are still visible
    minPlanetDisplaySize = 1000  -- km
    -- Add some padding (20% of window)
    safetyMargin = 0.8



-- get the diameter of the planet to display (use the planet and its ratio to earth then divide by scale factor)
getPlanetDisplayRadius :: Float -> Planet -> Float
getPlanetDisplayRadius scaleFactor planet = ((planetRadius planet) * (radiusRatio planet)) / (scaleFactor / 1000)


-- Scales the image of the planet approperatly in relation to the actual planetary radius
scalePlanetImage :: Float -> Planet -> Picture -> Picture
scalePlanetImage scaleFactor planet img = scale planetScaleFactor planetScaleFactor img
    where 
        planetScaleFactor = (getPlanetDisplayRadius scaleFactor planet) / imgRadius



-- set the inital starting state of the simulation 
initialSimState :: Planet -> Altitude -> Velocity -> Eccentricity -> Picture -> Picture -> SimState
initialSimState p alt vel ecc pImg sImg = MkSimState p alt vel ecc pImg sImg 0 1



-- places all necessary pictures on window,, based on scaling and orbital calculations
makeSim :: Float -> SimState -> Picture
makeSim scaleFactor sim = scale (zoom sim) (zoom sim) (pictures [translate 0 0 (scalePlanetImage scaleFactor (currentPlanet sim) (planetImg sim))  -- compose planet image in center of window with the SimState planet and planet image
                                   , color white (scale 1 (sma / orbitalRadiM) (circle (orbitalRadiM / scaleFactor))) -- draw orbital path
                                   , translate x y (scale (1/50) (1/50) (satImg sim)) -- sat image in place of orbit based on x & y location 
                                   , logInfoText sim -- log data text
                                   , optionText ]) -- instruction text
    where 
        orbitalRadiM = kmToMeters (planetRadius (currentPlanet sim)) + kmToMeters (altSim sim) -- orbital radius in Meters (planet radi + altitude of orbit)

        -- gather values to find the position of orbit from time t ;; utilizing equations in Equations.hs 
        velM = kmToMeters (velSim sim) -- velocity in meters
        period = orbitPeriod orbitalRadiM velM -- orbital period 
        w = 2 * pi / period -- angular velocity
        sma = semiMinorAxis orbitalRadiM (eccentricity sim) -- semiMinorAxis 

        -- x & y orbital position due to time
        x = (orbitalPosX orbitalRadiM w (timeT sim)) / scaleFactor
        y = (orbitalPosY sma w (timeT sim)) / scaleFactor
                                


-- Picture of the log info derived from the SimState
logInfoText :: SimState -> Picture
-- zipWith list of numbers [0..] to properly space out text
logInfoText sim  = pictures (zipWith go [0 ..] info)
    where
        -- writes text in given loctation (n changes causing spaced lines), scale text and set color
        go n infoLine = translate (-390) (-100 - n * 15) (scale 0.08 0.08 (color white (text infoLine)))
        -- format of text to list out
        info = [ "Planet ID: " ++ name
               , "Mass: " ++ mass
               , "Radius: " ++ radius
               , "Altitude: " ++ alt
               , "Velocity: " ++ vel
               , "Eccentricity: " ++ ecc
               , "Orbital Period: " ++ show (period / 3600) ++ " hr"]
            where 
                -- information needed from SimState for the log info
                name      = show (planetId (currentPlanet sim))
                mass      = show (planetMass (currentPlanet sim) / 1e24) ++ "e24 kg"
                radius    = show (planetRadius (currentPlanet sim)) ++ " km"
                alt       = show (altSim sim) ++ " km"
                vel       = show (velSim sim) ++ " km/s"
                ecc       = show (eccentricity sim)
                -- r and v to calculate period 
                r         = kmToMeters (planetRadius (currentPlanet sim)) + kmToMeters (altSim sim)
                v         = kmToMeters (velSim sim)
                period    = orbitPeriod r v



-- Creates Picture of user input control options 
optionText :: Picture
-- zipWith list of numbers [0..] to properly space out text
optionText = pictures (zipWith go [0 ..] options)
    where 
        -- writes text in given loctation (n changes causing spaced lines), scale text and set color
        go n optionLine = translate 200 (-100 - n * 15) (scale 0.08 0.08 (color white (text optionLine)))
        -- list of lines needed to be displayed (key with corresponding action)
        options = [ "Click the following key for..."
                  , "( 1 ) - Mercury"
                  , "( 2 ) - Venus"
                  , "( 3 ) - Earth"
                  , "( 4 ) - Mars"
                  , "( 5 ) - Jupiter"
                  , "( 6 ) - Saturn"
                  , "( 7 ) - Uranus"
                  , "( 8 ) - Neptune"
                  , "( 9 ) - Moon"
                  , "( Q ) - Increase Altitude"
                  , "( A ) - Decrease Altitude"
                  , "( W ) - Increase Velocity"
                  , "( S ) - Decrease Velocity"
                  , "( E ) - Increase Ecentricity"
                  , "( D ) - Decrease Altitude"]


-- increases time in order to simulate the orbital movement of the satellite 
updateSimTime :: Float -> SimState -> SimState
updateSimTime x (MkSimState a b c d e f t z) = MkSimState a b c d e f (t + x) z 



-- 
changePlanet :: Int -> SimState -> IO SimState
changePlanet pID s@(MkSimState p alt vel e pImage sImage t z) = case lookup pID planetMap of
  Just newPlanet -> do
    pImg <- handlePNG (planetImage newPlanet)
    return (MkSimState newPlanet alt vel e pImg sImage 0 z) 
  Nothing -> return s



changeParameter :: Char -> SimState -> IO SimState
changeParameter key (MkSimState p alt vel e pImage sImage t z) = return 
    (case key of
        'q' -> MkSimState p (alt + 100) vel e pImage sImage t z
        'a' -> MkSimState p (max 100 (alt - 100)) vel e pImage sImage t z 
        'w' -> MkSimState p alt (vel + 10) e pImage sImage t z 
        's' -> MkSimState p alt (max 10 (vel - 10)) e pImage sImage t z 
        'e' -> MkSimState p alt vel (min 1 (e + 0.1)) pImage sImage t z 
        'd' -> MkSimState p alt vel (max 0.1 (e - 0.1)) pImage sImage t z 
        _   -> MkSimState p alt vel e pImage sImage t z )
