module Cells
(
  generateCells,
)
where 
import Random
import GameDefinitions
import Strategy
import CellUtils


generateCell :: Position -> Radius -> Integer -> CreatePlayer -> StrategyFunction -> Cell
generateCell pos rad id True stFn = Player   pos rad id stFn
generateCell pos rad id False _   = Plankton pos rad id

randomPosition :: Seed -> Seed -> BoardSize -> Radius -> Position 
randomPosition s1 s2 boardSize cellRadius = Point x y
  where 
    x = randDoubleInRange s1 cellRadius (boardSize-cellRadius)
    y = randDoubleInRange s2 cellRadius (boardSize-cellRadius)

generateCells :: Seed -> Integer -> BoardSize -> Radius -> Bool -> [Cell] -> StrategyFunction -> [Cell]
generateCells seed noCells bS initRadius createPlayer existingCells stFn = generateCellsR (randomSeeds seed) noCells bS initRadius createPlayer existingCells stFn

generateCellsR :: [Seed] -> Integer -> BoardSize -> Radius -> CreatePlayer -> [Cell] -> StrategyFunction -> [Cell]
generateCellsR _ 0 _ _ _ xs _ = xs
generateCellsR (s1:s2:seeds) noCells bS initRadius createPlayer xs stFn = 
  let newPos = randomPosition s1 s2 bS initRadius in
  if doesntOverlapWithAnyCell newPos initRadius xs then
    generateCellsR seeds (noCells-1) bS initRadius createPlayer (xs++[generateCell newPos initRadius (fromIntegral(length xs)) createPlayer stFn]) stFn
  else 
    generateCellsR seeds noCells bS initRadius createPlayer xs stFn
