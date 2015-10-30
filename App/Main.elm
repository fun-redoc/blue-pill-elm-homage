module App.Main where



import Window
import Signal as S exposing ((<~),(~))
import App.Model exposing (..)
import App.Signal exposing (..)
import App.Update exposing (..)
import App.View exposing (..)

-- MAIN --
main = render <~ Window.dimensions ~ S.foldp stepGame defaultGame event
