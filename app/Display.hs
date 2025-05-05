module Display where

import Graphics.UI.Brillo
import Equations
import Orbit
import Graphics.Gloss.Juicy

--TODO:
--Create event handlers to handle button clicks and sliding of sliders
--Implement these functions
--Update orbit information based on user selections

--Would like to keep as much as possible in Display rather than Main

makeButton :: UI -> Planet -> IO (Planet, ButtonPicture)
        makeButton ui planet do
            planetPic <- loadJuicyPNG (planetImage planet)
            let image = case planetPic of
                Just planet -> scale (1/10) (1/10) planet
                Nothing -> color red (text "Error")
            button <- button ui image --Not sure if this actually works yet
            return (planet, button)

createPlanetButtons :: UI -> [Planet] -> IO [(Planet, Button Picture)]
createPlanetButtons ui planets = mapM (makeButton ui) planets

createSliders :: UI -> IO (Slider float, Slider float, Slider float) --Not sure if this actually works yet
createSliders ui = do
    velSlider <- slider ui (0, 10) 5 --FIX: Not sure exactly what these values should be
    altSlider <- slider ui (0, 10) 5 --FIX
    eccSlider <- slider ui (0, 10) 5 --FIX
    return (velSlider, altSlider, eccSlider)