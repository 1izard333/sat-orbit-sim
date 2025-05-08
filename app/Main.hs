module Main where

import Equations
import Orbit
import Brillo
import Display (planetButtons, displayButtons, volumeButtons)


-- planets list ( https://nssdc.gsfc.nasa.gov/planetary/factsheet/planet_table_ratio.html )

mercury :: Planet
mercury = MkPlanet 1 0.33010e24 2440.5 "resources/mercury.png"

venus :: Planet
venus = MkPlanet 2 4.8673e24 6051.8 "resources/venus.png"

earth :: Planet
earth = MkPlanet 3 5.9722e24 6378.137 "resources/earth.png"

mars :: Planet
mars = MkPlanet 4 0.64169e24 3396.2 "resources/mars.png"

jupiter :: Planet
jupiter = MkPlanet 5 1898.13e24 71492 "resources/jupiter.png"

saturn :: Planet
saturn = MkPlanet 6 568.32e24 60268 "resources/saturn.png"

uranus :: Planet
uranus = MkPlanet 7 86.811e24 25559 "resources/uranus.png"

neptune :: Planet
neptune = MkPlanet 8 102.409e24 24764 "resources/neptune.png"

moon :: Planet
moon = MkPlanet 0 0.07346e24 1738.1 "resources/moon.png"


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