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

{-- 
  Pasos a seguir en cada etapa de la simulacion:
  Asignar a las células sus velocidades según sus estrategias
  1) Asignar a las celulas velocidades segun sus estrategias
  2) Dar el estado del juego al escritor (de estar presente) que graba en disco el
  estado de la simulación (para realizar luego las animaciones)
  3) Mover el mundo un paso temporal, avanzando a todas las células según su velocidad
  4) Revisar los solapamientos de células con plankton y realizar las absorciones en caso de solapamiento
  5) Revisar los solapamientos de células con células y realizar las absorciones en caso de solapamiento
  6) Revisar la condición de equilibrio para determinar si el juego terminó
-}

startSimulation :: Seed -> Seed -> BoardSize -> NoPlayer -> Radius -> NoPlankton -> Radius -> [OutputFunction] -> MainReturnType
startSimulation s1 s2 bS noY rY noK rK outputters = runGame (GC bS players planktons outputters) (GH [players] [planktons])
  where
    players   = generateCells s1 noY bS rY True []
    planktons = generateCells s2 noK bS rK False players

runGame :: GameContainer -> GameHistory -> IO [()]
runGame (GC bS players planktons os) (GH playersHistory planktonsHistory) = do 
  let playersAfterMoving = movePlayers (players, planktons) bS
  let newGC = processOverlappingCells bS (playersAfterMoving, planktons) os
  if equilibriumReached newGC then
    (mapM (callOutputter (playersHistory++[getPlayers newGC]) (planktonsHistory++[getPlanktons newGC])) os)
  else
    runGame newGC (GH (playersHistory++[getPlayers newGC]) (planktonsHistory++[getPlanktons newGC])) 


  
equilibriumReached :: GameContainer -> Bool
equilibriumReached gc = length (getPlayers gc) == 1 || (length (getPlanktons gc) == 0 && not(playersCantEatEachOther (getPlayers gc)))

movePlayers :: GameState ->  BoardSize -> Players
movePlayers (players, planktons) bS = playersAfterMoving
  where
    newVelocities = applyStrategies (players,planktons)
    playersAfterMoving = applyNewVelocities bS players newVelocities

applyStrategies :: GameState -> [Vector]
applyStrategies (players, planktons) = map (\x -> (getStrategy x) x (players,planktons)) players 

callOutputter :: [Players] -> [Planktons] -> OutputFunction -> IO ()
callOutputter ys ks fn = fn ys ks
