module Strategy
(
  closestAgent,
  getStrategy,
)
where

import Game
import CellUtils

getStrategy :: Cell -> StrategyFunction
getStrategy (Player _ _ _ st) = closestAgent
getStrategy (Plankton _ _ _) = closestAgent

-- closestAgent player yS kS = minimum (filter (0 /=) (map ((getDistCells player), (yS++kS)))
closestAgent :: StrategyFunction
closestAgent player (y:yS) kS = getVectorFromCellToCell player (closestAgentR player randCell (yS++kS))
  where randCell = if getID y == getID player then yS!!0 else y

closestAgentR :: Cell -> Cell -> [Cell] -> Cell
closestAgentR player closestCell [] = closestCell
closestAgentR player closestCell (x:xs) = 
  if (getID player /= getID x) && isCloser (getDistCells player closestCell) player x then
    closestAgentR player x xs
  else
    closestAgentR player closestCell xs

isCloser :: Double -> Cell -> Cell -> Bool
isCloser prevDist c1 c2 = getDistCells c1 c2 < prevDist 
