module Main(main) where

import Graphics.Gloss -- to call play here

import Game -- game state logic
import Display -- rendering of game state
import Input -- keyboardFunction

main :: IO ()
main = play window background fps initialState render keyboardFunction update
