module App.Model  (GameState(..), defaultGameState, Game, defaultGame, gameOver, Player, defaultPlayer, Pill, defaultPill, defaultRadius, defaultColor, isGameOver, isLevelCompleted, nextLevel)  where

import Color as C exposing (Color)
import List exposing (..)
import App.Const exposing (..)
import App.Vec exposing (..)

defaultRadius = 15
defaultColor = C.lightRed
levelScoreTreshold = 5

type alias Pill = {pos:Vec, vel:Vec, rad:Float, col:C.Color}
defaultPill : Pill
defaultPill = { pos = (0,hHeight+defaultRadius)
              , vel = (0,-90)
              , rad = defaultRadius
              , col = C.lightRed }

type alias Player = Pill
defaultPlayer = { defaultPill | pos<-(0,0), col <- C.black }

type alias Level = Int
type alias Score = Int

type alias Game = { level:Level, score:Score, totalScore:Score, player:Player, pills:(List Pill), spawnPill:()->Pill}
defaultGame : Game
defaultGame = { level = 1
              , score = 0
              , totalScore = 0
              , player = defaultPlayer
              , pills = []--(map (\i-> {defaultPill|pos<-(i*3*defaultPill.rad,snd defaultPill.pos)}) [0..3])
              , spawnPill = (\_ -> defaultPill)
              }

type GameState = NewGame Game | Playing Game | Paused Game | GameOver Level Score | LevelCompleted Game
defaultGameState = NewGame defaultGame

isGameOver : Game -> Bool
isGameOver g = g.score < 0

gameOver : Game -> GameState
gameOver g = GameOver g.level (g.totalScore)

isLevelCompleted : Game -> Bool
isLevelCompleted {score} = score == levelScoreTreshold

nextLevel : Game ->Game
nextLevel g = { g | level <- g.level + 1, score <- 0, totalScore <- g.totalScore+g.score,  player <- defaultPlayer , pills <- []}
