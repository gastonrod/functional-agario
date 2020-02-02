module CellUtils
(
  getID,
  getPos,
  getX,
  getY,
  getRad,
  getDist,
  getDistCells,
  getVectorFromCellToCell,
  doesntOverlapWithAnyCell,
  cellOverlapsWithCell,
  overlapsWithCell,
  isPlayer,
  replaceCell,
)
where
import Game

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

cellOverlapsWithCell :: Cell -> Cell -> Bool
cellOverlapsWithCell c1 c2 = overlapsWithCell (getPos c1) (getRad c1) c2

-- todo: clean up this mess
doesntOverlapWithCell :: Position -> Radius -> Cell -> Bool
doesntOverlapWithCell pos rad cell = not (overlapsWithCell pos rad cell)
overlapsWithCell :: Position -> Radius -> Cell -> Bool
overlapsWithCell pos rad cell = getDist pos (getPos cell) <= rad + (getRad cell)

isPlayer :: Cell -> Bool
isPlayer (Player _ _ _ _) = True
isPlayer (Plankton _ _ _) = False

replaceCell :: [Cell] -> Cell -> [Cell]
replaceCell cells c = c:(filter (\x-> getID x /= getID c) cells)
