module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

width, height, offset :: Int
width = 600
height = 600
offset = 100

fps :: Int
fps = 60

window :: Display
window = InWindow "Trace Tracker" (width, height) (offset, offset)

background :: Color
background = black

data DrawMode = Mode {
    pos :: (Float, Float),
    direction :: Char
} deriving Show

initialState :: DrawMode
initialState = Mode {
    pos = (0, 0),
    direction = 'R'
}

render :: DrawMode -> Picture
render mode = pictures [ball]
              where
                ball = color ballColor $ circleSolid 10
                ballColor = dark red

update :: ViewPort -> Float -> DrawMode -> DrawMode
update _ 100 initialState = initialState

drawing :: Picture
drawing = pictures [ball, wall]
              where
                ball = color ballColor $ circleSolid 10               
                wall = color ballColor $ rectangleWire 400 400
                ballColor = dark red

main :: IO ()
main = display window background drawing
--main = simulate window background fps initialState render update
