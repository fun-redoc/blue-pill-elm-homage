module App.Main where

-- ## READY ##
-- pause and restart game using space bar
-- todo
-- game over when score falls below 0
-- restart after game over with new game
-- levels -> faster and more huddled
-- additional reward pills
-- save scores in local db

import Html as H
import Graphics.Element as E exposing (Element)
import Graphics.Collage as GC
import Window
import Mouse
import Keyboard
import Signal as S exposing ((<~),(~))
import Color as C exposing (Color)
import Time as T exposing (Time)
import List exposing (..)
import AnimationFrame
import Random
import Text

(width, height) = (400,400)
(hWidth, hHeight) = (width/2,height/2)
defaultRadius = 15
defaultColor = C.lightRed


type alias Vec = (Float, Float)
vecAdd : Vec->Vec->Vec
vecAdd (x1,y1) (x2,y2) = (x1+x2, y1+y2)
vecSub : Vec->Vec->Vec
vecSub (x1,y1) (x2,y2) = (x1-x2, y1-y2)
vecMulS : Vec -> Float -> Vec
vecMulS (x,y) t = (x*t,y*t)
vecLen : Vec -> Float
vecLen (x,y) = sqrt ((x*x)+(y*y))

type alias MousePosition = (Int, Int)
relativeMouse : MousePosition -> MousePosition -> MousePosition
relativeMouse (ox,oy) (x,y) = (x - ox, oy - y)

center : (Int,Int) -> (Int,Int)
center (x,y) = (x//2,y//2)

type alias Pill = {pos:Vec, vel:Vec, rad:Float, col:C.Color}
defaultPill : Pill
defaultPill = { pos = (0,hHeight+defaultRadius)
              , vel = (0,-90)
              , rad = defaultRadius
              , col = C.lightRed }
stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos (vecMulS p.vel t)}


type alias Player = Pill
defaultPlayer = { defaultPill | pos<-(0,0), col <- C.black }
stepPlayer : MousePosition -> Player -> Player
-- stepPlayer mp p = { p | pos <- (toFloat <| fst mp, toFloat <| snd mp)}
stepPlayer mp p = { p | pos <- (toFloat <| fst mp, snd p.pos)}

type alias Game = { score:Int, player:Player, pills:(List Pill)}
defaultGame : Game
defaultGame = { score = 0
              , player = defaultPlayer
              , pills = (map (\i-> {defaultPill|pos<-(i*3*defaultPill.rad,snd defaultPill.pos)}) [0..3])}


stepGame : Action -> Game -> Game
stepGame e g = 
  case e of
    Tick (t,mp) -> let hit pl pi = (pl.rad + pi.rad) > (vecLen <| vecSub pl.pos pi.pos)
                       culled {rad,pos} = ((snd pos)+rad) < (-hHeight)
                       (notHit,hitRed,hitBlue,culledBlue, culledPills) = foldl (\pil (nh,hr,hb,cb,cu) -> ( if (not (hit g.player pil)) && (not (culled pil)) then pil::nh else nh
                                                                                    , if (hit g.player pil) && (not (culled pil)) && pil.col == C.lightRed then pil::hr else hr
                                                                                    , if (hit g.player pil) && (not (culled pil)) && pil.col == C.lightBlue then pil::hb else hb
                                                                                    , if (culled pil) && pil.col == C.lightBlue then pil::cb else cb
                                                                                    , if (culled pil) then pil::cu else cu
                                                                                  )
                                                              ) ([],[],[],[],[]) g.pills
                    in { g | pills <- map (stepPill t) <| notHit -- filter (\pi -> (not (hit player pi)) && (not (culled pi))) pills
                           , player <- stepPlayer mp g.player
                           , score <- g.score+(length hitBlue)-(length hitRed)-(length culledBlue)
                         }
    Add pill    -> { g | pills <- pill :: g.pills }
    _           -> g -- DEFAULT NoOp

textForm : Float -> Float -> Float -> String -> GC.Form
textForm x y scl str = Text.fromString str |> Text.color C.grey
                                  |> E.centered
                                  |> GC.toForm
                                  |> GC.scale scl
                                  |> GC.move (x,y)

render : (Int, Int) -> Game -> Element
render (w,h) game = 
  let formPill {rad,col,pos} = GC.circle rad |> GC.filled col |> GC.move pos
      formText = textForm 0 0 3 ("Score: " ++ (toString game.score))
      formHowManyPills = (GC.toForm <| E.show (length game.pills))
      forms = formText :: formHowManyPills ::  (map formPill <| game.player :: game.pills)
  in  GC.collage width height forms |> E.color C.white  
                               |> E.container w h E.middle 
                               |> E.color C.lightGray

-- SIGNAL
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

-- EVENTS 
type Action = NoOp | Tick (Time,MousePosition) | Add Pill | Pause 

pauseOrRun : Signal Bool
pauseOrRun = Signal.foldp (\b s-> xor b s) False Keyboard.space

event = Signal.map2 (\r i -> if r then i else Pause) 
                    pauseOrRun
                    (S.mergeMany [ Tick <~ inputSignal
                                , Add <~ ((\maybeRandomX color-> Maybe.withDefault defaultPill 
                                                                                    (Maybe.map (\randomX->{defaultPill| pos <- (fst randomX,snd defaultPill.pos), col<-color}) maybeRandomX)
                                         )<~(randomXSignal  spawnSignal)~(randomColor spawnSignal))
                                ])

-- MAIN --
main = render <~ Window.dimensions ~ S.foldp stepGame defaultGame event
