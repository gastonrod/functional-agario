module CellUtils
(
  getID,
  getPos,
  getRad,
  getDist,
  getDistCells,
  getVectorFromCellToCell,
  doesntOverlapWithAnyCell,
  cellOverlapsWithCell,
  isPlayer,
  isSameCell,
  replaceCell,
  applyNewVelocities,
  move,
)
where
import GameDefinitions

isSameCell :: Cell -> Cell -> Bool
isSameCell (Player _ _ _ _) (Plankton _ _ _) = False
isSameCell (Plankton _ _ _) (Player _ _ _ _) = False
isSameCell  (Plankton _ _ id1) (Plankton _ _ id2) = id1 == id2
isSameCell  (Player _ _ id1 _) (Player _ _ id2 _) = id1 == id2

getID :: Cell -> Integer
getID (Player _ _ id _) = id
getID (Plankton _ _ id) = id

getPos :: Cell -> Position
getPos (Player (Point x y) _ _ _) = Point x y
getPos (Plankton (Point x y) _ _) = Point x y

getX :: Cell -> Double
getX (Player (Point x _) _ _ _) = x
getX (Plankton (Point x _) _ _) = x

getY :: Cell -> Double
getY (Player (Point _ y) _ _ _) = y
getY (Plankton (Point _ y) _ _) = y

getRad :: Cell -> Radius
getRad (Player   _ r _ _) = r
getRad (Plankton _ r _) = r

getDist :: Position -> Position -> Double
getDist (Point x1 y1) (Point x2 y2) = sqrt((x1-x2)^2+(y1-y2)^2)

getDistCells :: Cell -> Cell -> Double
getDistCells c1 c2 = getDist (getPos c1) (getPos c2)

getVectorFromCellToCell :: Cell -> Cell -> Vector
getVectorFromCellToCell c1 c2 = Vec (x/norm) (y/norm) where
  x = (getX c2) - (getX c1)
  y = (getY c2) - (getY c1)
  norm = sqrt(x^2 + y^2)

doesntOverlapWithAnyCell :: Position -> Radius -> [Cell] -> Bool
doesntOverlapWithAnyCell pos rad xs = all (doesntOverlapWithCell pos rad) xs

doesntOverlapWithCell :: Position -> Radius -> Cell -> Bool
doesntOverlapWithCell pos rad cell = getDist pos (getPos cell) > rad + (getRad cell)

cellOverlapsWithCell :: Cell -> Cell -> Bool
cellOverlapsWithCell c1 c2 = overlapsWithCell (getPos c1) (getRad c1) c2

overlapsWithCell :: Position -> Radius -> Cell -> Bool
overlapsWithCell pos rad cell = getDist pos (getPos cell) <= rad + (getRad cell)

isPlayer :: Cell -> Bool
isPlayer (Player _ _ _ _) = True
isPlayer (Plankton _ _ _) = False

replaceCell :: [Cell] -> Cell -> [Cell]
replaceCell cells c = c:(filter (isSameCell c) cells)

applyNewVelocities :: BoardSize -> [Cell] -> [Vector] -> [Cell]
applyNewVelocities _ [] [] = []
applyNewVelocities bS (Player pos rad id fun:ys) (v:vs) = [(Player (move bS pos v) rad id fun)] ++ applyNewVelocities bS ys vs

move :: BoardSize -> Position -> Vector -> Position
move bS (Point x1 y1) (Vec x2 y2) = (Point newX newY)
  where
    newX = max 0 (min (x1+x2) bS)
    newY = max 0 (min (y1+y2) bS)
