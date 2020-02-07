module EatingMechanics (
  canEat,
  isEating,
  consume,
  playersEatPlayers,
  playersEatPlanktonsReturnPlanktons,
  playersEatPlanktonsReturnPlayers,
  checkIfPlanktonIsEaten,
  processOverlappingCells,
  playersCantEatEachOther,
) where

import GameDefinitions
import Data.Maybe
import CellUtils

processOverlappingCells :: BoardSize -> GameState -> [OutputFunction] -> GameContainer
processOverlappingCells bs (players, planktons) os = (GC bs playersAfterEatingIsDone planktonsAfterEatingIsDone os)
  where
    playersAfterEatingPlayers  = playersEatPlayers players
    playersAfterEatingIsDone   = playersEatPlanktonsReturnPlayers (playersAfterEatingPlayers, planktons)
    planktonsAfterEatingIsDone = playersEatPlanktonsReturnPlanktons (playersAfterEatingPlayers, planktons)

playersCantEatEachOther :: Players -> Bool
playersCantEatEachOther ys = any (\y -> any (canEat y) ys ) ys

canEat :: Cell -> Cell -> Bool
canEat c1 c2 = getRad c1 > (getRad c2) * 1.1

isEating :: Cell -> Cell -> Bool
isEating player cell = cellOverlapsWithCell player cell && canEat player cell

consume :: Cell -> Cell -> Cell
consume (Player pos rad id strat) victim = (Player pos (sqrt(rad*rad+(getRad victim)*(getRad victim))) id strat)

playersEatPlanktonsReturnPlayers :: GameState -> Players
playersEatPlanktonsReturnPlayers (ys, ks) = playersEatPlanktonsReturnPlayersR (ys, ks) []

playersEatPlanktonsReturnPlayersR :: GameState -> Players -> Players
playersEatPlanktonsReturnPlayersR (players, []) _ = players
playersEatPlanktonsReturnPlayersR (allPlayers, (k:planktons)) _ =
  if isPlayer planktonIfNotEaten then
    playersEatPlanktonsReturnPlayersR ((replaceCell allPlayers planktonIfNotEaten), planktons) []
  else
    playersEatPlanktonsReturnPlayersR (allPlayers, planktons) []
  where
    planktonIfNotEaten = checkIfPlanktonIsEaten allPlayers k

playersEatPlanktonsReturnPlanktons :: GameState -> Players
playersEatPlanktonsReturnPlanktons (ys, ks) = playersEatPlanktonsReturnPlanktonsR (ys, ks) []

playersEatPlanktonsReturnPlanktonsR :: GameState -> Players -> Planktons
playersEatPlanktonsReturnPlanktonsR (_, []) survivingPlanktons = survivingPlanktons
playersEatPlanktonsReturnPlanktonsR (allPlayers, (k:planktons)) survivingPlanktons =
  if isPlayer planktonIfNotEaten then
    playersEatPlanktonsReturnPlanktonsR (allPlayers, planktons) survivingPlanktons
  else
    playersEatPlanktonsReturnPlanktonsR (allPlayers, planktons) (planktonIfNotEaten:survivingPlanktons)
  where
    planktonIfNotEaten = checkIfPlanktonIsEaten allPlayers k

checkIfPlanktonIsEaten :: Players -> Cell -> Cell
checkIfPlanktonIsEaten [] plankton = plankton
checkIfPlanktonIsEaten (x:xs) plankton =
  if cellOverlapsWithCell x plankton then
    consume x plankton
  else
    checkIfPlanktonIsEaten xs plankton
{-
f :: (a -> a -> Bool) -> [a] -> a -> a
f g [] a = a
f g (x:xs) a = if g x a then x else f g xs a
-}

playersEatPlayers :: Players -> Players
playersEatPlayers players = playersEatR players players []

playersEatR :: Players -> Players -> Players -> Players
playersEatR _ [] playersLeft = playersLeft
playersEatR allPlayers (y:players) acumPlayers =
  if isNothing playerAfterEating then
    playersEatR allPlayers players acumPlayers
  else
    playersEatR allPlayers players (fromJust playerAfterEating:acumPlayers)
  where
    playerAfterEating = playerEatCell allPlayers y

playerEatCell :: Players -> Cell -> Maybe Cell
playerEatCell [] curPlayer = Just curPlayer
playerEatCell (x:xs) curPlayer =
  if isEating x curPlayer then Nothing
  else if isEating curPlayer x then
    playerEatCell xs (consume curPlayer x)
  else
    playerEatCell xs curPlayer
