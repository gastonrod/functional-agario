module Game
(
  GameContainer(..),
  BoardSize,
  Radius,
  NoPlankton, 
  NoPlayer,
  Cell(..),
  Position(..),
  CellID,
  StrategyFunction,
  OutputFunction,
--  OutputFunctionToFile,
  Vector(..),
  CreatePlayer,
  getPlayers,
  getPlanktons,
  getOutputters,
)
where

data GameContainer = GC BoardSize [Cell] [Cell] [OutputFunction]
data Position = Point Double Double deriving Show
type BoardSize = Double
type Radius = Double
type NoPlankton = Integer
type NoPlayer = Integer
type CellType = Integer
data Cell = Player Position Radius Integer StrategyFunction | Plankton Position Radius Integer
type CellID = Int
data Vector = Vec Double Double deriving Show
type CreatePlayer = Bool

-- Player -> Players -> Planktons -> Vector
type StrategyFunction = Cell -> [Cell] -> [Cell] -> Vector
type OutputFunction = [Cell] -> [Cell] -> String
--type OutputFunctionToFile = FilePath -> OutputFunction

getPlayers :: GameContainer -> [Cell]
getPlayers (GC _ players _ _) = players
getPlanktons :: GameContainer -> [Cell]
getPlanktons (GC _ _ planktons _) = planktons
getOutputters :: GameContainer -> [OutputFunction]
getOutputters (GC _ _ _ outputters) = outputters
