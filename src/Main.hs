module Main(main) where

import Graphics.Gloss -- to call animate here

import Game -- game state logic
import Display -- rendering of game state




main :: IO ()
main = play window background fps initialState render keyboardFunction update
