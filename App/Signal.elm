module App.Signal (event) where

import Window
import Mouse
import Keyboard
import Signal as S exposing ((<~),(~))
import Color as C exposing (Color)
import Time as T exposing (Time)
import List exposing (..)
import AnimationFrame
import Random
import Char
import Set

import App.Vec exposing (..)
import App.Const exposing (..)
import App.Utils exposing (..)
import App.Model exposing (..)
import App.Action exposing (..)


-- SIGNALS --
--delta = T.fps 3
spawnSignal : Signal Time
spawnSignal = (T.every (T.second*0.5))
delta = AnimationFrame.frame

inputSignal = (,) <~ (T.inSeconds <~ delta)
                   ~  S.sampleOn delta (S.map2 relativeMouse (S.map center Window.dimensions) Mouse.position) -- the main signal is the time signal, the other signals are sampled on the time signal, when you try without the pill will not fall continuously but according to how fast the mouse is moved

randomX = (Random.float (-hWidth) hWidth) 
randomXSignal : Signal Time -> Signal (Maybe (Float, Random.Seed))
randomXSignal timeSignal = S.foldp (\t m-> case m of
                                  Nothing -> Just (Random.generate randomX (Random.initialSeed (round t))) 
                                  Just (rndx, seed) -> Just (Random.generate randomX seed))
                        Nothing timeSignal

randomRange = (Random.int 0 9)
randomColor' : Signal Time -> Signal ( Maybe ( Int, Random.Seed))
randomColor' timeSignal = S.foldp (\t m-> case m of
                                Nothing -> Just (Random.generate randomRange (Random.initialSeed (round t)))
                                Just (rndn,seed) -> Just (Random.generate randomRange seed)
                             ) Nothing timeSignal
randomColor : Signal Time -> Signal Color
randomColor timeSignal = (\m->Maybe.withDefault defaultColor (Maybe.map (\(v,_)->if v >= 2 then defaultColor else C.lightBlue) m ))<~(randomColor' timeSignal)

pauseOrRun : Signal Bool
pauseOrRun = Signal.foldp (\b s-> xor b s) False Keyboard.space

startGame : Signal Bool
startGame = (Signal.map (Set.member <| Char.toCode 'S')Keyboard.keysDown)

event = Signal.map3 (\s r i -> if s then StartGame else if r then i else Pause) 
                    startGame
                    pauseOrRun
                    (S.mergeMany [ Tick <~ inputSignal
                                , Add <~ ((\maybeRandomX color-> Maybe.withDefault defaultPill 
                                                                                    (Maybe.map (\randomX->{defaultPill| pos <- (fst randomX,snd defaultPill.pos), col<-color}) maybeRandomX)
                                         )<~(randomXSignal  spawnSignal)~(randomColor spawnSignal))
                                ])
