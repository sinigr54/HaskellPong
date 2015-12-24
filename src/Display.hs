module Display where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort -- viewport is required for update func

import Game

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)


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
	, mkPaddle rose $ player1Paddle game
	, mkPaddle orange $ player2Paddle game
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
			rectangleSolid 800 10

		wallColor = greyN 0.5
		walls = pictures [wall 300, wall (-300)]

		--  Make a paddle of a given border and vertical offset
		mkPaddle :: Color -> Position -> Picture
		mkPaddle col (x, y) = pictures
		  [ --translate x y $ color col $ rectangleSolid 26 86 -- borders
		   translate x y $ color paddleColor $ rectangleSolid widthPaddle heightPaddle -- filling
		  ]

		paddleColor = light (light blue)

-- Update the game by moving the ball.
-- Ignoring the ViewPort argument.
update :: Float -> PongGame -> PongGame
update seconds = paddleBounce . wallBounce . moveBall seconds
