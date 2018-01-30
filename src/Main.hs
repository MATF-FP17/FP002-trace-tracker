module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.String

import System.IO.Unsafe
import System.Random

--dimenzije i pozicija prozora
width, height, offset :: Int
width = 800
height = 600
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
render :: String -> DrawMode -> Picture
render option mode = pictures picArray
              where
                picArray = case option of "0" -> [trace1, pointer1]
                                          "2" -> [trace1, trace3, pointer1, pointer3]
                                          "4" -> [trace1, trace2, trace3, trace4, pointer1, pointer2, pointer3, pointer4]
                trace1 = color lineColor $ Line $ dots mode
                trace2 =  rotate (90) $ trace1
                trace3 =  rotate (180) $ trace1 
                trace4 =  rotate (270) $ trace1 
                pointer1 = translate (fst $ pointB mode) (snd $ pointB mode) $ color ballColor $ circleSolid 4
                pointer2 = rotate (90) $ pointer1
                pointer3 = rotate (180) $ pointer1
                pointer4 = rotate (270) $ pointer1
                lineColor = dark $ dark green
                ballColor = dark red

--funkcija koja vrsi izracunavanje koordinata nove linije
moveLine :: Float -> Float -> DrawMode -> DrawMode
moveLine lSize seconds  mode = mode { pointA = pointB mode, pointB = (x, y), dirLine = rest (dirLine mode), dots = dots mode ++ [(x, y)]} 
   where 
       dir = direction (dirLine mode)
       (x, y) = if dir == "U" 
                then (fst $ pointB mode, snd (pointB mode) + lSize) 
                else if dir == "D"
                     then (fst $ pointB mode, snd (pointB mode) - lSize) 
                     else if dir == "R"
                          then (fst (pointB mode) + lSize, snd $ pointB mode)
                          else if dir == "L"
                               then (fst (pointB mode) - lSize, snd $ pointB mode)    
                               else (fst $ pointB mode, snd $ pointB mode)
                     
rest :: String -> String
rest  [] = []
rest (x:xs) = xs

update :: Float -> ViewPort -> Float -> DrawMode -> DrawMode
update lSize _ seconds  = moveLine lSize seconds 

--funkcija koja provera ispravnost unete niske za iscrtavanje            
checkString :: String -> Bool
checkString [] = True
checkString (x:xs) = if x == 'U' || x == 'D' || x == 'R' || x == 'L'
                     then checkString xs 
                     else False    

--pomocni poziv koji prima dodatni argument koji sluzi za prosledjivanje nase niske
callSim traceLine lSize option = simulate window background fps (initialState traceLine) (render option) (update lSize)                    

main :: IO ()
main = do 
    putStrLn "Odaberite velicinu koraka (S-standardni, L-manji, G-veci):"
    lineSize <- getLine
    putStrLn "Unesite nisku za iscrtavanje:"  
    traceLine <- getLine
    putStrLn "Da li zelite sliku zarotiranu 0, 2 ili 4 puta?"
    option <- getLine
    let lSize = if (lineSize == "L") 
                then 12.5 
                else if (lineSize == "G") 
                     then 37.5 
                     else 25.0                   
    if null traceLine
    then putStrLn "Trazi se niska oblika UDLR (U-up, D-down, L-left, R-right)" 
    else if checkString traceLine 
         then callSim traceLine lSize option
         else putStrLn "Neispravna niska. Pokusajte ponovo."
