module App.Action (Action(..)) where

import Time as T exposing (Time)

import App.Model exposing (Pill)
import App.Utils exposing (..)

-- ACTIONS -- 
type Action = NoOp | Tick (Time,MousePosition) | Add Pill | Pause 
