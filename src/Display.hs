module Display where

import Graphics.Gloss

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
	, mkPaddle $ player1Paddle game
	, mkPaddle $ player2Paddle game
	]
	where
		-- the ball
		ball = uncurry translate (ballLocation game) $ color ballColor $ circleSolid 10
		ballColor = white

		--  make a paddle of a given border and vertical offset
		mkPaddle :: Position -> Picture
		mkPaddle (x, y) = translate x y $ color paddleColor $ rectangleSolid widthPaddle heightPaddle -- filling

		paddleColor = white

		score = translate (-30) (260) $ Scale 0.3 0.3 $ color white $ text ((show $ player2Score game) ++ ":" ++ (show $ player1Score game))

-- update the game by moving the ball and paddles.
update :: Float -> PongGame -> PongGame
update seconds = scoreBounce . paddleBounce . wallBounce . moveBall seconds . movePaddles seconds
