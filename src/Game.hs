module Game where

import Graphics.Gloss.Interface.Pure.Game

-- state of pong game
data PongGame = Game
	{ ballLocation :: (Float, Float) 	-- x, y of the pong ball location
	, ballVelocity :: (Float, Float) 	-- x, y of the pong ball velocity
	, player1Paddle :: (Float, Float)	-- left player paddle location
										-- Zero is middle of screen
	, player2Paddle :: (Float, Float)	-- right player paddle height
	} deriving Show


-- window stats
width, height, offset :: Int
width = 800
height = 600
offset = 100

-- paddleStats
widthPaddle, heightPaddle :: Float
widthPaddle = 20
heightPaddle = 80

-- ball-related type alliases
type Radius = Float
type Width = Float
type Height = Float
type Position = (Float, Float)


--init game with starting state
initialState :: PongGame
initialState = Game
	{ ballLocation = (-10, 30)
	, ballVelocity = (100, -200)
	, player1Paddle = (385, 150)
	, player2Paddle = (-385, 0)
	}


-- | Update the ball position using its current velocity.
moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game { ballLocation = (x', y') }
  where
    -- Old locations and velocities
    (x, y) = ballLocation game
    (vx, vy) = ballVelocity game
    -- New locations
    x' = x + vx * seconds
    y' = y + vy * seconds

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral height / 2
    bottomCollision = y + radius >=  fromIntegral height / 2

paddleRightCollision :: Position -> Position -> Width -> Bool
paddleRightCollision (xb, yb) (xp, yp) w = collision
 	where
		height = yp
 		collision = xb - w <= xp && yb <= height && yb >= (height - heightPaddle)

paddleLeftCollision :: Position -> Position -> Width -> Bool
paddleLeftCollision (xb, yb) (xp, yp) w = collision
 	where
		height = yp
 		collision = xb + w >= xp && yb <= height && yb >= (height - heightPaddle)

-- Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVelocity = (vx', vy') }
	where
		w = widthPaddle
		(vx, vy) = ballVelocity game

		vx' = if (paddleLeftCollision (ballLocation game) (player1Paddle game) w)
			|| (paddleRightCollision (ballLocation game) (player2Paddle game) w)
			then
				-vx
			else
				vx
		vy' = vy

-- Detect a collision with one of the side walls. Upon collisions,
-- change the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVelocity = (vx, vy') }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities.
    (vx, vy) = ballVelocity game

    vy' = if wallCollision (ballLocation game) radius
          then
             -- Update the velocity.
             -vy
           else
            -- Do nothing. Return the old velocity.
            vy

keyboardFunction :: Event -> PongGame -> PongGame
keyboardFunction (EventKey (Char 's') _ _ _) game =
  game { ballLocation = (0, 0) }
keyboardFunction _ game = game
