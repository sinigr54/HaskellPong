module Display where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort -- viewport is required for update func

import Game

window :: Display
window = InWindow "PONG" (200,200) (10,10)

-- frames per second
fps :: Int
fps = 60

background :: Color
background = dark $ dark $ dark blue		
		

-- draw the current state
render :: PongGame -> Picture
render game = 
	pictures 
	[ ball, walls
	, mkPaddle rose 120 $ player1 game
	, mkPaddle orange (-120) $ player2 game  
	]
	where
		-- The ball
		ball = uncurry translate (ballLocation game) $ color ballColor $ circleSolid 10
		ballColor = dark red
		
		--  The bottom and top walls
		wall :: Float -> Picture
		wall offset =
			translate 0 offset $
			color wallColor $
			rectangleSolid 270 10
			
		wallColor = greyN 0.5
		walls = pictures [wall 150, wall (-150)]

		--  Make a paddle of a given border and vertical offset
		mkPaddle :: Color -> Float -> Float -> Picture
		mkPaddle col x y = pictures
		  [ translate x y $ color col $ rectangleSolid 26 86
		  , translate x y $ color paddleColor $ rectangleSolid 20 80
		  ]

		paddleColor = light (light blue)
		
-- Update the game by moving the ball.
-- Ignoring the ViewPort argument.
update :: ViewPort -> Float -> PongGame -> PongGame
update _ = moveBall 