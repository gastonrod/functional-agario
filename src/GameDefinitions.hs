module GameDefinitions
(
  GameContainer(..),
  GameHistory(..),
  BoardSize,
  Radius,
  NoPlankton, 
  NoPlayer,
  Cell(..),
  Position(..),
  CellID,
  StrategyFunction,
  OutputFunction,
  GameStateToStringFunction,
  Vector(..),
  CreatePlayer,
  getPlayers,
  getPlanktons,
  getOutputters,
)
where

type Players   = [Cell]
type Planktons = [Cell]
data GameContainer = GC BoardSize Players Planktons [OutputFunction]
data GameHistory = GH [Players] [Planktons]
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

type StrategyFunction = Cell -> Players -> Planktons -> Vector
type OutputFunction = [Players] -> [Planktons] -> IO ()
type GameStateToStringFunction = Players -> Planktons -> String

getPlayers :: GameContainer -> Players
getPlayers (GC _ players _ _) = players

getPlanktons :: GameContainer -> Planktons
getPlanktons (GC _ _ planktons _) = planktons

getOutputters :: GameContainer -> [OutputFunction]
getOutputters (GC _ _ _ outputters) = outputters
