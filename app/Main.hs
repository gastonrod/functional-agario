module Main where

import Lib
import Random
import Game
import Cells
import CellUtils
import Outputter
import Strategy
import Motor

--players = [Player (Point 2 2) 2.0 1 closestAgentGreedy, Player (Point 6 2) 2.0 2 closestAgentGreedy,Player (Point 6 6) 2.0 3 closestAgentGreedy]
c1 = Player (Point 1 1) 1.0 1 planktonFirstGreedy
c2 = Player (Point 30 30) 2.0 3 planktonFirstGreedy
c3 = Player (Point 20 20) 3.0 5 closestAgentGreedy
p1 = Plankton (Point 25 25) 0.5 2
p2 = Plankton (Point 25 30) 0.5 4
p3 = Plankton (Point 0 10) 0.5 6
players = [c1, c2]
planktons = [p1, p2, p3]
s1 = 130
s2 = 222
boardSize = 100
noY = 15
rY = 1.0
noK = 200
rK = 0.5
outputters = [writeCsvOutputter "./runs/count_2.csv" countPlayersAndPlanktonInState countPlayersAndPlanktonInStateHeader,writeFileOutputter "./runs/r2.xyz" stateToXyzFormat]
gc = (GC boardSize players planktons outputters)

main :: IO [()]
main = do
  startSimulation s1 s2 boardSize noY rY noK rK  outputters
  --runGame gc (GH [players] [planktons])
