module Strategy
(
  closestAgentGreedy,
  planktonFirstGreedy,
  playerFirstGreedy,
  getStrategy,
)
where

import GameDefinitions
import CellUtils
import EatingMechanics

closestAgentGreedy :: StrategyFunction
closestAgentGreedy player ((y:yS), kS) = getVectorFromCellToCell player (closestAgentGreedyR player randCell (yS++kS))
  where randCell = if isSameCell y player then yS!!0 else y

closestAgentGreedyR :: Cell -> Cell -> [Cell] -> Cell
closestAgentGreedyR player closestCell [] = closestCell
closestAgentGreedyR player closestCell (x:xs) = 
  if not (isSameCell player x) && isCloser closestCell player x && canEat player x then
    closestAgentGreedyR player x xs
  else
    closestAgentGreedyR player closestCell xs

isCloser :: Cell -> Cell -> Cell -> Bool
isCloser c1 c2 c3 = getDistCells c2 c3 < (getDistCells c1 c2)
isCloserM :: Cell -> Cell -> Cell -> Bool
isCloserM c1 c2 c3 = getDistCells c2 c3 < (getDistCells c1 c2)

planktonFirstGreedy :: StrategyFunction 
planktonFirstGreedy player (ys, []) = closestAgentGreedy player (ys, [])
planktonFirstGreedy player (ys, (k:ks)) = getVectorFromCellToCell player (planktonFirstGreedyR player k ks)

planktonFirstGreedyR :: Cell -> Cell -> [Cell] -> Cell
planktonFirstGreedyR player closestK [] = closestK
planktonFirstGreedyR player closestK (k:ks) = 
  if isCloser closestK player k then
    closestAgentGreedyR player k ks
  else
    closestAgentGreedyR player closestK ks


playerFirstGreedy :: StrategyFunction 
playerFirstGreedy player (ys, ks) = getVectorFromCellToCell player (playerFirstGreedyR player Nothing  (ys++ks))

playerFirstGreedyR :: Cell -> Maybe Cell -> [Cell] -> Cell
playerFirstGreedyR player Nothing ((Plankton pos rad id):xs) = planktonFirstGreedyR player (Plankton pos rad id) xs
playerFirstGreedyR player (Just (Player pos rad id strat)) ((Plankton _ _ _):xs) = player
playerFirstGreedyR player Nothing ((Player pos rad id f):xs) =
  if canEat player x then
    playerFirstGreedyR player (Just x) xs
  else
    playerFirstGreedyR player Nothing xs
  where
    x = (Player pos rad id f)
playerFirstGreedyR player (Just (Player pos1 rad1 id1 f1)) ((Player pos2 rad2 id2 f2):xs) =
  if not (isSameCell player x) && isCloser closestCell player x && canEat player x then
    playerFirstGreedyR player (Just x) xs
  else
    playerFirstGreedyR player (Just closestCell) xs
  where
    closestCell = (Player pos1 rad1 id1 f1)
    x = (Player pos2 rad2 id2 f2)

getStrategy :: Cell -> StrategyFunction
getStrategy (Player _ _ _ st) = st
getStrategy (Plankton _ _ _) = closestAgentGreedy
