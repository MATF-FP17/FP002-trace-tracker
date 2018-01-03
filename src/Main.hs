module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

width, height, offset :: Int
width = 600
height = 600
offset = 100
traceLine = []

fps :: Int
fps = 60

window :: Display
window = InWindow "Trace Tracker" (width, height) (offset, offset)

background :: Color
background = black

data DrawMode = Mode {
    pointA :: (Float, Float),
    pointB :: (Float, Float)
} deriving Show

direction :: String -> Char
direction [] = 'F'
direction (x:xs) = x  

initialState :: DrawMode
initialState = Mode {
    pointA = (0, 0),
    pointB = (0, 0)
}

render :: DrawMode -> Picture
render mode = pictures [trace]
              where
                trace = color ballColor $ Line [pointA mode, pointB mode]
                ballColor = dark red

moveLine :: Float -> DrawMode -> DrawMode
moveLine sec mode = mode { pointA = pointB mode, pointB = (x, y)} 
   where
    dir = direction traceLine
    (x, y) = if dir == 'U'
             then (fst $ pointB mode, snd $ pointB mode + 25) 
             else if dir == 'D'
                  then (fst $ pointB mode, snd $ pointB mode - 25) 
             else if dir == 'R'
                  then (fst $ pointB mode + 25, snd $ pointB mode)
             else (fst $ pointB mode - 25, snd $ pointB mode)

     

update :: ViewPort -> Float -> DrawMode -> DrawMode
update _ = moveLine

            
checkString :: String -> Bool
checkString [] = True
checkString (x:xs) = if x == 'U' || x == 'D' || x == 'R' || x == 'L'
                   then checkString xs 
                   else False    
                
main :: IO ()
main = do  
    putStrLn "Unesite nisku za iscrtavanje:"  
    traceLine <- getLine  
    if null traceLine
    then putStrLn "Trazi se niska oblika UDLR (U-up, D-down, L-left, R-right)" 
    else if checkString traceLine
        then simulate window background fps initialState render update
        else putStrLn "Neispravna niska. Pokusajte ponovo"
