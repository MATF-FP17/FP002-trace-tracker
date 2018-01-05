module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.String

import System.IO.Unsafe
import System.Random

--dimenzije i pozicija prozora
width, height, offset :: Int
width = 850
height = 850
offset = 100

fps :: Int
fps = 2

window :: Display
window = InWindow "Trace Tracker" (width, height) (offset, offset)

background :: Color
background = black

--struktura koriscena za iscrtavanje, sadrzi pocetnu i krajnju tacku,
-- pravce u kojima treba da se icrtava i listu iscrtanih linija
data DrawMode = Mode {
    pointA :: (Float, Float),
    pointB :: (Float, Float),
    dirLine :: String,
    dots :: [(Float, Float)]
} deriving Show

--pomocna funkcija koja vraca trenutni pravac koji treba da se iscrta
--ako se dodje do kraja, sa "F" signalizira kraj iscrtavanja
direction :: String -> String
direction [] = "F"
direction (x:xs) = [x]

--pocetno stanje (koordinatni pocetak)
initialState :: String -> DrawMode
initialState traceLine = Mode {
    pointA = (0, 0),
    pointB = (0, 0),
    dirLine = traceLine,
    dots = [(0,0)]
}

--funkcija koja vrsi iscrtavanje liste tacaka koje su do sad obradjene (dots)
render :: DrawMode -> Picture
render mode = pictures [trace]
              where
                trace = color lineColor $ Line $ dots mode
                lineColor = dark green
                --r = unsafePerformIO (getStdRandom (randomR (0, 255)))
                --r = randomR (0, 255)
                --r = 0
                --lineColor = makeColor r r r 0

--funkcija koja vrsi izracunavanje koordinata nove linije
moveLine :: Float -> DrawMode -> DrawMode
moveLine seconds mode = mode { pointA = pointB mode, pointB = (x, y), dirLine = rest (dirLine mode), dots = dots mode ++ [(x, y)]} 
   where 
       dir = direction (dirLine mode)
       (x, y) = if dir == "U" 
            then (fst $ pointB mode, snd $ pointB mode + 25) 
            else if dir == "D"
                  then (fst $ pointB mode, snd $ pointB mode - 25) 
            else if dir == "R"
                  then (fst $ pointB mode + 25, snd $ pointB mode)
            else if dir == "L"
                  then (fst $ pointB mode - 25, snd $ pointB mode)    
            else (fst $ pointB mode, snd $ pointB mode)
                     
rest :: String -> String
rest  [] = []
rest (x:xs) = xs

update :: ViewPort -> Float -> DrawMode -> DrawMode
update _ seconds = moveLine seconds

--funkcija koja provera ispravnost unete niske za iscrtavanje            
checkString :: String -> Bool
checkString [] = True
checkString (x:xs) = if x == 'U' || x == 'D' || x == 'R' || x == 'L'
                   then checkString xs 
                   else False    

--pomocni poziv koji prima dodatni argument koji sluzi za prosledjivanje nase niske
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