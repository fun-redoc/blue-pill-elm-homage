module App.Utils where

type alias MousePosition = (Int, Int)
relativeMouse : MousePosition -> MousePosition -> MousePosition
relativeMouse (ox,oy) (x,y) = (x - ox, oy - y)

center : (Int,Int) -> (Int,Int)
center (x,y) = (x//2,y//2)


