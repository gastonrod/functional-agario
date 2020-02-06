module Motor
(
  startSimulation,
  applyStrategies,
  runGame,
)
where
import Game
import Cells
import CellUtils
import Random
import Strategy
import Outputter
import EatingMechanics

{-- Asignar a las células sus velocidades según sus estrategias
1) Asignar a las celulas velocidades segun sus estrategias
2) Dar el estado del juego al escritor (de estar presente) que graba en disco el
estado de la simulación (para realizar luego las animaciones)
3) Mover el mundo un paso temporal, avanzando a todas las células según su velocidad
4) Revisar los solapamientos de células con plankton y realizar las absorciones en caso de solapamiento
5) Revisar los solapamientos de células con células y realizar las absorciones en caso de solapamiento
6) Revisar la condición de equilibrio para determinar si el juego terminó
-}

startSimulation :: Seed -> Seed -> BoardSize -> NoPlayer -> Radius -> NoPlankton -> Radius -> [OutputFunction] -> IO [()]
startSimulation s1 s2 bS noY rY noK rK (o:os)= runGame (GC bS players planktons (o:os)) (GH [players] [planktons])
  where
    players   = generateCells s1 noY bS rY True []
    planktons = generateCells s2 noK bS rK False players

runGame :: GameContainer -> GameHistory -> IO [()]
runGame (GC bS players planktons os) (GH playersHistory planktonsHistory) = do 
  let newVelocities = applyStrategies players planktons
  let playersAfterMoving = applyNewVelocities bS players newVelocities
  let playersAfterEatingPlayers = playersEatPlayers playersAfterMoving
  let playersAfterEatingPlankton = playersEatPlanktonsReturnPlayers playersAfterEatingPlayers planktons
  let planktonsAfterBeingEaten = playersEatPlanktonsReturnPlanktons playersAfterEatingPlayers planktons
  let newGC = GC bS playersAfterEatingPlankton planktonsAfterBeingEaten os
  if equilibriumReached newGC then
    (mapM (callOutputter (playersHistory++[playersAfterEatingPlankton]) (planktonsHistory++[planktonsAfterBeingEaten])) os)
  else
    runGame newGC (GH (playersHistory++[playersAfterEatingPlankton]) (planktonsHistory++[planktonsAfterBeingEaten])) 
  
equilibriumReached :: GameContainer -> Bool
equilibriumReached gc = length (getPlayers gc) == 1

applyStrategies :: [Cell] -> [Cell] -> [Vector]
applyStrategies players planktons = map (\x -> (getStrategy x) x players planktons) players 

callOutputter :: [[Cell]] -> [[Cell]] -> OutputFunction -> IO ()
callOutputter ys ks fn = fn ys ks

applyNewVelocities :: BoardSize -> [Cell] -> [Vector] -> [Cell]
applyNewVelocities _ [] [] = []
applyNewVelocities bS (Player pos rad id fun:ys) (v:vs) = [(Player (move bS pos v) rad id fun)] ++ applyNewVelocities bS ys vs

move :: BoardSize -> Position -> Vector -> Position
move bS (Point x1 y1) (Vec x2 y2) = (Point newX newY)
  where
    newX = max 0 (min (x1+x2) bS)
    newY = max 0 (min (y1+y2) bS)
