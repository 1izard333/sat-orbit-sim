module Display where

import Graphics.UI.Brillo
import Equations
import Orbit
import Graphics.Gloss.Juicy

createPlanetButtons :: UI -> [Planet] -> IO [(Planet, Button Picture)]
createPlanetButtons ui planets = mapM (makeButton ui) planets
    where
        makeButton :: UI -> Planet -> IO (Planet, ButtonPicture)
        makeButton ui planet do
            planetPic <- loadJuicyPNG (planetImage planet)
            let image = case planetPic of
                Just planet -> scale (1/10) (1/10) planet
                Nothing -> color red (text "Error")
            button <- button ui image
            return (planet, button)
