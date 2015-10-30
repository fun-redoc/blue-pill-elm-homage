module App.Update (stepGame) where

import Signal as S exposing ((<~),(~))
import Color as C exposing (Color)
import Time as T exposing (Time)
import List exposing (..)

import App.Vec   exposing (..)
import App.Const exposing (..)
import App.Utils exposing (..)
import App.Model exposing (..)
import App.Action exposing (..)
import App.Signal exposing (..)

-- UPDATE --
stepPlayer : MousePosition -> Player -> Player
-- stepPlayer mp p = { p | pos <- (toFloat <| fst mp, toFloat <| snd mp)}
stepPlayer mp p = { p | pos <- (toFloat <| fst mp, snd p.pos)}

stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos (vecMulS p.vel t)}


stepGame : Action -> Game -> Game
stepGame e g = 
  if e /= StartGame && isGameOver g 
     then g 
     else case e of
            Tick (t,mp) -> let hit pl pi = (pl.rad + pi.rad) > (vecLen <| vecSub pl.pos pi.pos)
                               culled {rad,pos} = ((snd pos)+rad) < (-hHeight)
                               (notHit,hitRed,hitBlue,culledBlue, culledPills) = 
                                 foldl (\pil (nh,hr,hb,cb,cu) -> ( if (not (hit g.player pil)) && (not (culled pil)) then pil::nh else nh
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
            StartGame   -> defaultGame
            _           -> g -- DEFAULT NoOp
