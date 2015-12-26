module Input where

import Game
import Graphics.Gloss.Interface.Pure.Game -- event keys

keyboardFunction :: Event -> PongGame -> PongGame
keyboardFunction (EventKey (SpecialKey KeySpace) _ _ _) game = game { player1Paddle = (p1X, p1Y), player2Paddle = (p2X, p2Y), ballLocation = (ballX, ballY), ballVelocity=(300,-300) }

keyboardFunction (EventKey (SpecialKey KeyUp) _ _ _) game = if (player1Up game == False) then game { player1Up = True, player1Down = False } else game { player1Up = False } --game { player1Paddle = (vx, vy') }

keyboardFunction (EventKey (SpecialKey KeyDown) _ _ _) game = if (player1Down game == False) then game { player1Down = True, player1Up = False } else game { player1Down = False } --game { player1Paddle = (vx, vy') }

keyboardFunction (EventKey (Char 'w') _ _ _) game = if (player2Up game == False) then game { player2Up = True, player2Down = False } else game { player2Up = False } --game { player2Paddle = (vx, vy') }

keyboardFunction (EventKey (Char 's') _ _ _) game = if (player2Down game == False) then game { player2Down = True, player2Up = False } else game { player2Down = False } --game { player2Paddle = (vx, vy') }

keyboardFunction _ game = game
