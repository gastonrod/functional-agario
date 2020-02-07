module Main where

import Random
import GameDefinitions
import Cells
import CellUtils
import Outputter
import Strategy
import Motor

s1 = 130 -- seed for players
s2 = 222 -- seed for planktons
boardSize = 100
noY = 15 -- number of players
rY = 1.0 -- initial radius of players
noK = 200 -- number of planktons
rK = 0.5 -- planktons radius
outputters = [
  writeCsvOutputter "./runs/count_2.csv" countPlayersAndPlanktonInState countPlayersAndPlanktonInStateHeader,
  writeFileOutputter "./runs/r2.xyz" stateToXyzFormat
  ]

main :: MainReturnType
main = do
  startSimulation s1 s2 boardSize noY rY noK rK  outputters closestAgentGreedy
