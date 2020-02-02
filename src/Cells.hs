module Cells
(
  randomPosition,
  randomPositions,
  generateCells,
  getVectorFromCellToCell,
)
where 
import Random
import Game
import Strategy
import CellUtils


generateCell :: Position -> Radius -> Integer -> CreatePlayer -> StrategyFunction -> Cell
generateCell pos rad id True stFn = Player   pos rad id stFn
generateCell pos rad id False _   = Plankton pos rad id

getClosest :: Cell -> [Cell] -> Position
getClosest (Player   pos _ _ _) xs = getClosestR (-1.0) (Point (-1) (-1)) pos xs
getClosest (Plankton pos _ _) xs = getClosestR (-1.0) (Point (-1) (-1)) pos xs

getClosestR :: Double -> Position -> Position -> [Cell] -> Position
getClosestR _ minPos _ [] = minPos 
getClosestR minDist minPos cellPos (x:xs) = 
  let distToCell =  (getDist (getPos x) cellPos) in
  if minDist == -1 then
    getClosestR distToCell (getPos x) cellPos xs
  else if distToCell < minDist then
    getClosestR distToCell (getPos x) cellPos xs
  else 
    getClosestR minDist minPos cellPos xs

randomPosition :: Seed -> Seed -> BoardSize -> Radius -> Position 
randomPosition s1 s2 boardSize cellRadius = Point x y
  where 
    x = randDoubleInRange s1 cellRadius (boardSize-cellRadius)
    y = randDoubleInRange s2 cellRadius (boardSize-cellRadius)

randomPositions :: Seed -> Integer -> [Position]
randomPositions seed init = rPR (randomSeeds seed) init []

rPR :: [Seed] -> Integer -> [Position] -> [Position]
rPR _ 0 xs = xs
rPR (s1:s2:seeds) size xs = rPR seeds (size-1) (randomPosition s1 s2 20 1.0 : xs)

generateCells :: Seed -> Integer -> BoardSize -> Radius -> CreatePlayer -> [Cell] -> [Cell]
generateCells seed noCells bS initRadius createPlayer  existingCells = generateCellsR (randomSeeds seed) noCells bS initRadius createPlayer existingCells

generateCellsR :: [Seed] -> Integer -> BoardSize -> Radius -> CreatePlayer -> [Cell] -> [Cell]
generateCellsR _ 0 _ _ _ xs = xs
generateCellsR (s1:s2:seeds) noCells bS initRadius createPlayer xs = 
  let newPos = randomPosition s1 s2 bS initRadius in
  if doesntOverlapWithAnyCell newPos initRadius xs then
    generateCellsR seeds (noCells-1) bS initRadius createPlayer (xs++[generateCell newPos initRadius (fromIntegral(length xs)) createPlayer closestAgent])
  else 
    generateCellsR seeds noCells bS initRadius createPlayer xs
