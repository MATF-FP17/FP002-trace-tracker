module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.String

width, height, offset :: Int
width = 600
height = 600
offset = 100

fps :: Int
fps = 1

window :: Display
window = InWindow "Trace Tracker" (width, height) (offset, offset)

background :: Color
background = black

data DrawMode = Mode {
    pointA :: (Float, Float),
    pointB :: (Float, Float),
    dirLine :: String
} deriving Show

direction :: String -> String
direction [] = "F"
direction (x:xs) = [x]

initialState :: String -> DrawMode
initialState traceLine = Mode {
    pointA = (0, 0),
    pointB = (0, 0),
    dirLine = traceLine
}

render :: DrawMode -> Picture
render mode = pictures [trace]
              where
                trace = color lineColor $ Line [pointA mode, pointB mode]
                lineColor = dark red

moveLine :: Float -> DrawMode -> DrawMode
moveLine seconds mode = mode { pointA = pointB mode, pointB = (x, y), dirLine = rest (dirLine mode)} 
   where 
       dir = direction (dirLine mode)
       (x, y) = if dir == "U" 
            then (fst $ pointB mode, snd $ pointB mode + 25) 
            else if dir == "D"
                  then (fst $ pointB mode, snd $ pointB mode - 25) 
            else if dir == "R"
                  then (fst $ pointB mode + 25, snd $ pointB mode)
                  else (fst $ pointB mode - 25, snd $ pointB mode)    
                  
rest :: String -> String
rest  [] = []
rest (x:xs) = xs

update :: ViewPort -> Float -> DrawMode -> DrawMode
update _ seconds = moveLine seconds

            
checkString :: String -> Bool
checkString [] = True
checkString (x:xs) = if x == 'U' || x == 'D' || x == 'R' || x == 'L'
                   then checkString xs 
                   else False    

callSim traceLine = simulate window background fps (initialState traceLine) render update                    
                   
main :: IO ()
main = do  
    putStrLn "Unesite nisku za iscrtavanje:"  
    traceLine <- getLine 
    if null traceLine
    then putStrLn "Trazi se niska oblika UDLR (U-up, D-down, L-left, R-right)" 
    else if checkString traceLine
        then callSim traceLine
        else putStrLn "Neispravna niska. Pokusajte ponovo"
        
