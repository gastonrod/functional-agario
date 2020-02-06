module EatingMechanics (
  canEat,
  isEating,
  consume,
  playersEatPlayers,
  playersEatPlanktonsReturnPlanktons,
  playersEatPlanktonsReturnPlayers,
  checkIfPlanktonIsEaten,
) where

import Game
import Data.Maybe
import CellUtils

canEat :: Cell -> Cell -> Bool
canEat c1 c2 = getRad c1 > (getRad c2) * 1.1

isEating :: Cell -> Cell -> Bool
isEating player cell = cellOverlapsWithCell player cell && (getRad player > ((getRad cell) * 1.1))

consume :: Cell -> Cell -> Cell
consume (Player pos rad id strat) victim = (Player pos (sqrt(rad*rad+(getRad victim)*(getRad victim))) id strat)

playersEatPlanktonsReturnPlayers :: [Cell] -> [Cell] -> [Cell]
playersEatPlanktonsReturnPlayers ys ks = playersEatPlanktonsReturnPlayersR ys ks []

playersEatPlanktonsReturnPlayersR :: [Cell] -> [Cell] -> [Cell] -> [Cell]
playersEatPlanktonsReturnPlayersR players [] _ = players
playersEatPlanktonsReturnPlayersR allPlayers (k:planktons) _ =
  if isPlayer planktonIfNotEaten then
    playersEatPlanktonsReturnPlayersR (replaceCell allPlayers planktonIfNotEaten) planktons []
  else
    playersEatPlanktonsReturnPlayersR allPlayers planktons []
  where
    planktonIfNotEaten = checkIfPlanktonIsEaten allPlayers k

playersEatPlanktonsReturnPlanktons :: [Cell] -> [Cell] -> [Cell]
playersEatPlanktonsReturnPlanktons ys ks = playersEatPlanktonsReturnPlanktonsR ys ks []

playersEatPlanktonsReturnPlanktonsR :: [Cell] -> [Cell] -> [Cell] -> [Cell]
playersEatPlanktonsReturnPlanktonsR _ [] survivingPlanktons = survivingPlanktons
playersEatPlanktonsReturnPlanktonsR allPlayers (k:planktons) survivingPlanktons =
  if isPlayer planktonIfNotEaten then
    playersEatPlanktonsReturnPlanktonsR allPlayers planktons survivingPlanktons
  else
    playersEatPlanktonsReturnPlanktonsR allPlayers planktons (planktonIfNotEaten:survivingPlanktons)
  where
    planktonIfNotEaten = checkIfPlanktonIsEaten allPlayers k

checkIfPlanktonIsEaten :: [Cell] -> Cell -> Cell
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

playersEatPlayers :: [Cell] -> [Cell]
playersEatPlayers players = playersEatR players players []

playersEatR :: [Cell] -> [Cell] -> [Cell] -> [Cell]
playersEatR _ [] playersLeft = playersLeft
playersEatR allPlayers (y:players) acumPlayers =
  if isNothing playerAfterEating then
    playersEatR allPlayers players acumPlayers
  else
    playersEatR allPlayers players (fromJust playerAfterEating:acumPlayers)
  where
    playerAfterEating = playerEatCell allPlayers y

playerEatCell :: [Cell] -> Cell -> Maybe Cell
playerEatCell [] curPlayer = Just curPlayer
playerEatCell (x:xs) curPlayer =
  if isEating x curPlayer then Nothing
  else if isEating curPlayer x then
    playerEatCell xs (consume curPlayer x)
  else
    playerEatCell xs curPlayer
