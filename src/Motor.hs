module Motor
(
  startSimulation,
  runGame,
)
where
import GameDefinitions
import Cells
import CellUtils
import Random
import Strategy
import Outputter
import EatingMechanics

startSimulation :: Seed -> Seed -> BoardSize -> NoPlayer -> Radius -> NoPlankton -> Radius -> [OutputFunction] -> StrategyFunction -> MainReturnType
startSimulation s1 s2 bS noY rY noK rK outputters stFn = runGame (GC bS players planktons outputters) ([players], [planktons])
  where
    players   = generateCells s1 noY bS rY True [] stFn
    planktons = generateCells s2 noK bS rK False players stFn

runGame :: GameContainer -> GameHistory -> IO [()]
runGame (GC bS players planktons os) (playersHistory, planktonsHistory) = do 
  let playersAfterMoving = movePlayers (players, planktons) bS
  let newGC = processOverlappingCells bS (playersAfterMoving, planktons) os
  if equilibriumReached newGC then
    (mapM (callOutputter ((playersHistory++[getPlayers newGC]), (planktonsHistory++[getPlanktons newGC]))) os)
  else
    runGame newGC ((playersHistory++[getPlayers newGC]), (planktonsHistory++[getPlanktons newGC])) 


  
equilibriumReached :: GameContainer -> Bool
equilibriumReached gc = length (getPlayers gc) == 1 || (length (getPlanktons gc) == 0 && not(playersCantEatEachOther (getPlayers gc)))

movePlayers :: GameState ->  BoardSize -> Players
movePlayers (players, planktons) bS = playersAfterMoving
  where
    newVelocities = applyStrategies (players,planktons)
    playersAfterMoving = applyNewVelocities bS players newVelocities

applyStrategies :: GameState -> [Vector]
applyStrategies (players, planktons) = map (\x -> (getStrategy x) x (players,planktons)) players 

callOutputter :: GameHistory -> OutputFunction -> IO ()
callOutputter gh fn = fn gh
