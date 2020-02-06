module Outputter
(
  cellToXyzFormat,
  stateToXyzFormat,
  writeFileOutputter,
  writeCsvOutputter,
  countPlayersAndPlanktonInState,
  countPlayersAndPlanktonInStateHeader,
)
where
import GameDefinitions

writeCsvOutputter :: String -> GameStateToStringFunction -> String -> OutputFunction
writeCsvOutputter filePath fn header = (\y -> \k -> writeFile filePath (concatenateWholeGameStateWithHeader header fn y k))

concatenateWholeGameStateWithHeader :: String -> GameStateToStringFunction -> [Players] -> [Planktons] -> String
concatenateWholeGameStateWithHeader  header f ys ks = header ++ concatenateWholeGameState f ys ks

writeFileOutputter :: String -> GameStateToStringFunction -> OutputFunction
writeFileOutputter filePath fn = (\y -> \k -> writeFile filePath (concatenateWholeGameState fn y k))

concatenateWholeGameState :: GameStateToStringFunction -> [Players] -> [Planktons] -> String
concatenateWholeGameState f [] [] = ""
concatenateWholeGameState f (y:ys) (k:ks) =  f y k ++ concatenateWholeGameState f ys ks

countPlayersAndPlanktonInStateHeader = "players,planktons\n"
countPlayersAndPlanktonInState :: GameStateToStringFunction
countPlayersAndPlanktonInState [] [] = ""
countPlayersAndPlanktonInState ys ks = show (length ys) ++ "," ++ show (length ks) ++ "\n"

stateToXyzFormat :: GameStateToStringFunction
stateToXyzFormat [] [] = ""
stateToXyzFormat ys ks = (show (length ys+length ks) ++ "\n" ++ (id foldl (++) "" (cellsInXyz ys ks)) ++ "\n")

cellsInXyz :: Players -> Planktons -> [String]
cellsInXyz ys ks = map cellToXyzFormat ys ++ map cellToXyzFormat ks 

cellToXyzFormat :: Cell -> String
cellToXyzFormat (Player (Point x y) rad id _) = "\n1 " ++ show x ++ " " ++ show y ++ " 1 " ++ show rad ++ " " ++ show id ++ " 255 0 0"
cellToXyzFormat (Plankton (Point x y) rad id) = "\n2 " ++ show x ++ " " ++ show y ++ " 1 " ++ show rad ++ " " ++ show id ++ " 0 255 0"

