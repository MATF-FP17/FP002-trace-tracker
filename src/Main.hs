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
                
checkString :: String -> Bool
checkString [] = True
checkString (x:xs) = if x == 'U' || x == 'D' || x == 'R' || x == 'L'
                   then checkString xs 
                   else False    
                
main :: IO ()
main = do  
    putStrLn "Unesite nisku za iscrtavanje:"  
    line <- getLine  
    if null line
    then putStrLn "Trazi se niska oblika UDLR (U-up, D-down, L-left, R-right)" 
    else if checkString line
        then display window background drawing
        else putStrLn "Neispravna niska. Pokusajte ponovo"
--main = simulate window background fps initialState render update
