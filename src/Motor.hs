module Motor
(
  startSimulation,
  applyStrategies,
  runGame,
  playersEatPlayers,
  playersEatPlanktonsReturnPlanktons,
  playersEatPlanktonsReturnPlayers,
  checkIfPlanktonIsEaten,
  eats,
)
where
import Game
import Cells
import CellUtils
import Random
import Strategy
import Outputter
import Data.Maybe

{-- Asignar a las células sus velocidades según sus estrategias
1) Asignar a las celulas velocidades segun sus estrategias
2) Dar el estado del juego al escritor (de estar presente) que graba en disco el
estado de la simulación (para realizar luego las animaciones)
3) Mover el mundo un paso temporal, avanzando a todas las células según su velocidad
4) Revisar los solapamientos de células con plankton y realizar las absorciones en caso de solapamiento
5) Revisar los solapamientos de células con células y realizar las absorciones en caso de solapamiento
6) Revisar la condición de equilibrio para determinar si el juego terminó
-}
startSimulation :: Seed -> Seed -> BoardSize -> NoPlayer -> Radius -> NoPlankton -> Radius -> [OutputFunction] -> String
startSimulation s1 s2 bS noY rY noK rK (o:os)= runGame (GC bS players planktons (o:os)) (o players planktons)
--startSimulation s1 s2 bS noY rY noK rK (o:os)= (o players planktons)
  where
    players   = generateCells s1 noY bS rY True []
    planktons = generateCells s2 noK bS rK False players

runGame :: GameContainer -> String -> String
runGame (GC bS players planktons (o:[])) op = do 
  let newVelocities = applyStrategies players planktons
  let playersAfterMoving = applyNewVelocities players newVelocities
  let playersAfterEatingPlayers = playersEatPlayers playersAfterMoving
  let playersAfterEatingPlankton = playersEatPlanktonsReturnPlayers playersAfterEatingPlayers planktons
  let planktonsAfterBeingEaten = playersEatPlanktonsReturnPlanktons playersAfterEatingPlayers planktons
  let newGC = GC bS playersAfterEatingPlankton planktonsAfterBeingEaten (o:[])
  if equilibriumReached newGC then
    (op++o playersAfterEatingPlankton planktonsAfterBeingEaten)
  else
    runGame newGC (op++o playersAfterEatingPlankton planktonsAfterBeingEaten)
  
equilibriumReached :: GameContainer -> Bool
equilibriumReached gc = length (getPlayers gc) == 1

{-
 - Para cada jugador tengo que ver a quienes se come, a los que no se come los dejo en una lista de los "vivos". Si el jugador es comido, devuelvo la lista entera porque no come a nadie.
 - necesito: todos los jugadores, a quien estoy analizando, los que sobreviven
 - -}

playersEatPlanktonsReturnPlayers :: [Cell] -> [Cell] -> [Cell]
playersEatPlanktonsReturnPlayers ys ks = playersEatPlanktonsReturnPlayersR ys ks []

playersEatPlanktonsReturnPlayersR :: [Cell] -> [Cell] -> [Cell] -> [Cell]
playersEatPlanktonsReturnPlayersR players [] _ = players
playersEatPlanktonsReturnPlayersR allPlayers (k:planktons) _ = 
  if isPlayer planktonIfNotEaten then 
    playersEatPlanktonsReturnPlayersR (replaceCell allPlayers planktonIfNotEaten) planktons []
  else
    playersEatPlanktonsReturnPlayersR allPlayers planktons []
  where
    planktonIfNotEaten = checkIfPlanktonIsEaten allPlayers k

playersEatPlanktonsReturnPlanktons :: [Cell] -> [Cell] -> [Cell]
playersEatPlanktonsReturnPlanktons ys ks = playersEatPlanktonsReturnPlanktonsR ys ks []

playersEatPlanktonsReturnPlanktonsR :: [Cell] -> [Cell] -> [Cell] -> [Cell]
playersEatPlanktonsReturnPlanktonsR _ [] survivingPlanktons = survivingPlanktons
playersEatPlanktonsReturnPlanktonsR allPlayers (k:planktons) survivingPlanktons = 
  if isPlayer planktonIfNotEaten then 
    playersEatPlanktonsReturnPlanktonsR allPlayers planktons survivingPlanktons
  else
    playersEatPlanktonsReturnPlanktonsR allPlayers planktons (planktonIfNotEaten:survivingPlanktons)
  where
    planktonIfNotEaten = checkIfPlanktonIsEaten allPlayers k

checkIfPlanktonIsEaten :: [Cell] -> Cell -> Cell
checkIfPlanktonIsEaten [] plankton = plankton
checkIfPlanktonIsEaten (x:xs) plankton = 
  if cellOverlapsWithCell x plankton then
    eat x plankton
  else
    checkIfPlanktonIsEaten xs plankton
{-
f :: (a -> a -> Bool) -> [a] -> a -> a
f g [] a = a
f g (x:xs) a = if g x a then x else f g xs a
-}

playersEatPlayers :: [Cell] -> [Cell]
playersEatPlayers players = playersEatR players players []
    
playersEatR :: [Cell] -> [Cell] -> [Cell] -> [Cell]
playersEatR _ [] playersLeft = playersLeft
playersEatR allPlayers (y:players) acumPlayers = 
  if isNothing playerAfterEating then 
    playersEatR allPlayers players acumPlayers
  else
    playersEatR allPlayers players (fromJust playerAfterEating:acumPlayers)
  where
    playerAfterEating = playerEatCell allPlayers y

playerEatCell :: [Cell] -> Cell -> Maybe Cell
playerEatCell [] curPlayer = Just curPlayer
playerEatCell (x:xs) curPlayer = 
  if eats x curPlayer then Nothing
  else if eats curPlayer x then
    playerEatCell xs (eat curPlayer x)
  else
    playerEatCell xs curPlayer

eats :: Cell -> Cell -> Bool
eats player cell = cellOverlapsWithCell player cell && (getRad player > getRad cell) 

eat :: Cell -> Cell -> Cell
eat (Player pos rad id strat) victim = (Player pos (sqrt(rad*rad+(getRad victim)*(getRad victim))) id strat)

applyStrategies :: [Cell] -> [Cell] -> [Vector]
applyStrategies players planktons = map (\x -> (getStrategy x) x players planktons) players 

callOutputter :: [Cell] -> [Cell] -> OutputFunction -> String
callOutputter ys ks fn = fn ys ks

applyNewVelocities :: [Cell] -> [Vector] -> [Cell]
applyNewVelocities [] [] = []
applyNewVelocities (Player (Point x1 y1) rad id fun:ys) (Vec x2 y2:vs) = [(Player (Point (x1+x2) (y1+y2)) rad id fun)] ++ applyNewVelocities ys vs

