module App.Model  (Game, defaultGame, Player, defaultPlayer, Pill, defaultPill, defaultRadius, defaultColor, isGameOver)  where

import Color as C exposing (Color)
import List exposing (..)
import App.Const exposing (..)
import App.Vec exposing (..)

defaultRadius = 15
defaultColor = C.lightRed

type alias Pill = {pos:Vec, vel:Vec, rad:Float, col:C.Color}
defaultPill : Pill
defaultPill = { pos = (0,hHeight+defaultRadius)
              , vel = (0,-90)
              , rad = defaultRadius
              , col = C.lightRed }

type alias Player = Pill
defaultPlayer = { defaultPill | pos<-(0,0), col <- C.black }

type alias Game = { score:Int, player:Player, pills:(List Pill)}
defaultGame : Game
defaultGame = { score = 0
              , player = defaultPlayer
              , pills = (map (\i-> {defaultPill|pos<-(i*3*defaultPill.rad,snd defaultPill.pos)}) [0..3])}

isGameOver : Game -> Bool
isGameOver g = g.score < 0
