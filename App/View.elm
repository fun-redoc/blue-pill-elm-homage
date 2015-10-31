module App.View where

import Html as H
import Graphics.Element as E exposing (Element)
import Graphics.Collage as GC
import Color as C exposing (Color)
import List exposing (..)
import Text

import App.Const exposing (..)
import App.Model exposing (..)

-- VIEW --
textForm : Float -> Float -> Float -> String -> GC.Form
textForm x y scl str = Text.fromString str |> Text.color C.grey
                                  |> E.centered
                                  |> GC.toForm
                                  |> GC.scale scl
                                  |> GC.move (x,y)

render : (Int, Int) -> GameState -> Element
render (w,h) gameState = 
  case gameState of
    (GameOver level score) -> let formText = textForm 0 0 3 ("Game Over " ++ (toString (level,score)))
                              in  GC.collage width height [formText] |> E.color C.white |> E.container w h E.middle |> E.color C.lightGray

    (LevelCompleted game) -> let formText = textForm 0 0 3 ("Level Completed " ++ (toString game.level))
                             in  GC.collage width height [formText] |> E.color C.white |> E.container w h E.middle |> E.color C.lightGray

    (Paused game)  -> let formText = textForm 0 0 3 "Paused"
                      in  GC.collage width height [formText] |> E.color C.white |> E.container w h E.middle |> E.color C.lightGray
    (NewGame game) -> let formText = textForm 0 0 3 "New Game"
                      in  GC.collage width height [formText] |> E.color C.white |> E.container w h E.middle |> E.color C.lightGray
    (Playing game) -> let formPill {rad,col,pos} = GC.circle rad |> GC.filled col |> GC.move pos
                          formText = if isGameOver game then textForm 0 0 3 "Game Over" else textForm 0 0 3 ("Score: " ++ (toString game.score))
                          formHowManyPills = (GC.toForm <| E.show (length game.pills))
                          forms = formText :: formHowManyPills ::  (map formPill <| game.player :: game.pills)
                      in  GC.collage width height forms |> E.color C.white  
                                                   |> E.container w h E.middle 
                                                   |> E.color C.lightGray
