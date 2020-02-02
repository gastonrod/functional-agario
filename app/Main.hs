module Main where

import Lib
import Random
import Game
import Cells
import CellUtils
import Outputter
import Strategy
import Motor

--players = [Player (Point 2 2) 2.0 1 closestAgent, Player (Point 6 2) 2.0 2 closestAgent,Player (Point 6 6) 2.0 3 closestAgent]
c1 = Player (Point 2 2) 2.0 1 closestAgent
c2 = Player (Point 5 5) 3.0 3 closestAgent
c3 = Player (Point 20 20) 3.0 5 closestAgent
c2e = Player (Point 1 1) 5.0 3 closestAgent
p1 = Plankton (Point 1.5 1.5) 1.5 2
p2 = Plankton (Point 7 7) 1.5 4
p3 = Plankton (Point 10 10) 1.5 6
players = [Player (Point 2 2) 2.0 1 closestAgent, Player (Point 6 6) 2.0 3 closestAgent]
planktons = [Plankton (Point 10 20) 1.0 2, Plankton (Point 60 60) 1.0 4]
players2 = [c1, c2, c3]
planktons2 = [p1, p2, p3]
s1 = 1
s2 = 2
boardSize = 250
noY = 2
rY = 3.0
noK = 2
rK = 1.0
--outputters = [toXyzFormatToFile "First_output.xyz", toXyzFormatToFile "Second_output.xyz"]
outputters = [toXyzFormat]
gc = (GC boardSize players2 planktons outputters)

main :: IO ()
main = do
--  print (generateCells 1 5 50 2.0 False (generateCells 1 5 50 2.0 True []))
--  putStrLn $ id (foldl (++) "" (map (\x-> x players planktons) [toXyzFormat, toXyzFormat]))
  putStrLn $ id (startSimulation s1 s2 boardSize 20 1.5 200 1.0 outputters)
  --putStrLn $ id (foldl (++) "" (map cellToXyzFormat (replaceIfIsPlayer players2 c2e)))
