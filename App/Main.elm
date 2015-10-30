module App.Main where

-- DOING 
-- when the count falls unter 0 the game should end

-- TODO
-- i should be able to start a new game
-- there should be levels -> quicker fall, more huddled with red
-- the level score should be saved locally
-- the red pills should fall in different velocities
-- the pills should accelerate like by gravity
-- black pill should show up in start
-- I schould be able to handle the game by coursor
-- the game should be available on iOs
-- the game should be avialable on Android
-- the gravity shoul fit the inclination of the device


import Window
import Signal as S exposing ((<~),(~))
import App.Model exposing (..)
import App.Signal exposing (..)
import App.Update exposing (..)
import App.View exposing (..)

-- MAIN --
main = render <~ Window.dimensions ~ S.foldp stepGame defaultGame event
