
module Common where

import Data.Map

type Coord = (Int,Int)
type State = Map Coord Bool

limx :: Int
limx = 8

limy :: Int
limy = 8

coords :: [Coord]
coords = [(x,y) | y <- [0..limx-1], x <- [0..limy-1]]
