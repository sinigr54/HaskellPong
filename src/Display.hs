module Display where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort -- viewport is required for update func

import Game

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

-- frames per second
fps :: Int
fps = 100

background :: Color
background = black

-- draw the current state
render :: PongGame -> Picture
render game =
	pictures
	[ ball, score
	, mkPaddle rose $ player1Paddle game
	, mkPaddle orange $ player2Paddle game
	]
	where
		-- the ball
		ball = uncurry translate (ballLocation game) $ color ballColor $ circleSolid 10
		ballColor = white

		--  make a paddle of a given border and vertical offset
		mkPaddle :: Color -> Position -> Picture
		mkPaddle col (x, y) = translate x y $ color paddleColor $ rectangleSolid widthPaddle heightPaddle -- filling
		
		score = translate (-30) (260) $ Scale 0.3 0.3 $ color white $ text ((show $ player1Score game) ++ ":" ++ (show $ player2Score game))

		paddleColor = white

-- update the game by moving the ball and paddles.
update :: Float -> PongGame -> PongGame
update seconds = paddleBounce . wallBounce . moveBall seconds . movePaddles seconds
