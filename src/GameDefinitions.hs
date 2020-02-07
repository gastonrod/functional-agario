module GameDefinitions
(
  MainReturnType,
  GameContainer(..),
  GameHistory(..),
  BoardSize,
  Radius,
  NoPlankton, 
  NoPlayer,
  Cell(..),
  Players,
  Planktons,
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
  GameState,
)
where

type Players   = [Cell]
type Planktons = [Cell]
type GameState = (Players, Planktons)
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
type MainReturnType = IO [()]

type StrategyFunction = Cell -> GameState -> Vector
type OutputFunction = [Players] -> [Planktons] -> IO ()
type GameStateToStringFunction = GameState -> String

getPlayers :: GameContainer -> Players
getPlayers (GC _ players _ _) = players

getPlanktons :: GameContainer -> Planktons
getPlanktons (GC _ _ planktons _) = planktons

getOutputters :: GameContainer -> [OutputFunction]
getOutputters (GC _ _ _ outputters) = outputters
