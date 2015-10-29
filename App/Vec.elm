module App.Vec where

type alias Vec = (Float, Float)
vecAdd : Vec->Vec->Vec
vecAdd (x1,y1) (x2,y2) = (x1+x2, y1+y2)
vecSub : Vec->Vec->Vec
vecSub (x1,y1) (x2,y2) = (x1-x2, y1-y2)
vecMulS : Vec -> Float -> Vec
vecMulS (x,y) t = (x*t,y*t)
vecLen : Vec -> Float
vecLen (x,y) = sqrt ((x*x)+(y*y))
