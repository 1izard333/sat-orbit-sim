module Equations where

{-
Current resources for equations:
** https://www.physicsclassroom.com/class/circles/lesson-4/mathematics-of-satellite-motion ** 
** Space Mission Analysis and Design book ** 
-}

type Position = (Float, Float)

-- Universal gravitational constant (N*m^2/kg^2)
gravConst :: Float 
gravConst = 6.6743e-11

-- Orbital Speed Equation (v = (GM/R)^(1/2))
velocity :: Float -> Float -> Float 
velocity massP radiO = sqrt((gravConst * massP) / radiO)

-- Centripetal acceleration (a = GM/R^2)
acceleration :: Float -> Float -> Float 
acceleration massP radiO = (gravConst * massP) / (radiO ** 2.0)

-- Orbital Period (T = 2piR/v)
orbitPeriod :: Float -> Float -> Float 
orbitPeriod radiO v = (2 * pi * radiO) / v

-- Altitude (alt = (GM/a)^(1/2) - R_planet)
altitude :: Float -> Float -> Float -> Float 
altitude massP acc radiP = (sqrt ((gravConst * massP) / acc )) - radiP

-- Semi-minor axis = (a^2 )
semiMinorAxis :: Float -> Float -> Float
semiMinorAxis radiO ecc = sqrt ((radiO ** 2) - ((ecc ** 2)*(radiO ** 2)))

-- convert km to m
kmToMeters :: Float -> Float
kmToMeters a = a * 1000  
      
-- orbital path/trajectory at time t
orbitPosition :: Float -> Float -> Float -> Float -> Position
orbitPosition a b w t = (x, y)
  where
    x = a * cos (w * t)
    y = b * sin (w * t)