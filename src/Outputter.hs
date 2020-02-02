module Outputter
(
  toXyzFormat,
  cellToXyzFormat,
--  toXyzFormatToFile,
)
where
import Game

{-
toXyzFormatToFile :: OutputFunctionToFile
toXyzFormatToFile filePath ys ks = do  
  writeFile filePath xyzString
  where
  xyzString = toXyzFormat ys ks
-}

toXyzFormat :: OutputFunction
toXyzFormat [] [] = ""
toXyzFormat ys ks = (show (length ys+length ks) ++ "\n\n" ++ (id foldl (++) "" (cellsInXyz ys ks)))

cellsInXyz :: [Cell] -> [Cell] -> [String]
cellsInXyz ys ks = map cellToXyzFormat ys ++ map cellToXyzFormat ks 

cellToXyzFormat :: Cell -> String
cellToXyzFormat (Player (Point x y) rad id _) = "1 " ++ show x ++ " " ++ show y ++ " 1 " ++ show rad ++ " " ++ show id ++ "255 0 0\n"
cellToXyzFormat (Plankton (Point x y) rad id) = "2 " ++ show x ++ " " ++ show y ++ " 1 " ++ show rad ++ " " ++ show id ++ "0 255 0\n"

