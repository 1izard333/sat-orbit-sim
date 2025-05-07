module Display where

import Brillo
import Brillo.Data.Picture
import Brillo.Data.Color
import Equations
import Orbit

data PlanetButton = PlanetButton {
    label :: String,
    posX :: Float,
    posY :: Float,
    radius :: Float,
    pColor :: Color
} deriving Show

planetButtons :: [PlanetButton]
planetButtons =
    [ PlanetButton "Mercury" (-300) 250 30 blue,
      PlanetButton "Venus" (-225) 250 30 rose,
      PlanetButton "Earth" (-150) 250 30 green,
      PlanetButton "Mars" (-75) 250 30 red,
      PlanetButton "Jupiter" (0) 250 30 orange,
      PlanetButton "Saturn" (75) 250 30 yellow,
      PlanetButton "Uranus" (150) 250 30 cyan,
      PlanetButton "Neptune" (225) 250 30 violet,
      PlanetButton "Moon" (300) 250 30 white
    ]

displayButton :: PlanetButton -> Picture
displayButton button =
    translate (posX button) (posY button) (
        pictures [
            color (pColor button) (circleSolid (radius button)),
            translate (-20) (-10) (scale (1/10) (1/10) (color black (text (label button))))
        ]
    )

displayButtons :: [PlanetButton] -> Picture
displayButtons ps = pictures (map displayButton ps)