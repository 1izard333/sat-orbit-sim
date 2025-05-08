module Display where

import Brillo
import Brillo.Data.Picture
import Brillo.Data.Color
import Equations
import Orbit

--data type for planet buttons
data PlanetButton = PlanetButton {
    label :: String,
    posX :: Float,
    posY :: Float,
    radius :: Float,
    pColor :: Color
} deriving Show

--data type for increase/decrease buttons (vel, acc, ecc)
data VolumeButton = VolumeButton {
    plusMinus :: String,
    positionX :: Float,
    positionY :: Float,
    rColor :: Color
}

--creates list of planet buttons
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

--creates list of increase/decrease buttons
volumeButtons :: [VolumeButton]
volumeButtons =
    [VolumeButton "-" (-250) (-250) white,
     VolumeButton "+" (-150) (-250) white,
     VolumeButton "-" (-50) (-250) white,
     VolumeButton "+" (50) (-250) white,
     VolumeButton "-" (150) (-250) white,
     VolumeButton "+" (250) (-250) white
     ]

--labels lower buttons with velocity, acceleration, and eccentricity
volumeLabels :: [(String, Float, Float)]
volumeLabels =
    [
        ("Velocity", (-217), -255),
        ("Acceleration", (-35), -255),
        ("Eccentricty", 168, -255)
    ]

--display each label
displayVolumeLabel :: (String, Float, Float) -> Picture
displayVolumeLabel (txt, x, y) =
    translate x y (scale (1/10) (1/10) (color white (text txt)))

--displays each planet button
displayPlanetButton :: PlanetButton -> Picture
displayPlanetButton button =
    translate (posX button) (posY button) (
        pictures [
            color (pColor button) (circleSolid (radius button)),
            translate (-20) (-10) (scale (1/10) (1/10) (color black (text (label button))))
        ]
    )

--displays each increase/decrease button
displayVolumeButton :: VolumeButton -> Picture
displayVolumeButton button =
    translate (positionX button) (positionY button) (
        pictures [
            color (rColor button) (rectangleSolid 20 20),
            translate (-15) (-10) (scale (1/5) (1/5) (color black (text (plusMinus button))))
        ]
    )

--displays all buttons at once
displayButtons :: [PlanetButton] -> [VolumeButton] -> Picture
displayButtons ps vs = pictures (
    map displayPlanetButton ps ++ 
    map displayVolumeButton vs ++
    map displayVolumeLabel volumeLabels
    )