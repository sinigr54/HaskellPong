module Game where

import Graphics.Gloss.Interface.Pure.Game

-- state of pong game
data PongGame = Game
	{ ballLocation :: (Float, Float) 	-- x, y of the pong ball location
	, ballVelocity :: (Float, Float) 	-- x, y of the pong ball velocity
	, player1Paddle :: (Float, Float)	-- left player paddle location
										-- Zero is middle of screen
	, player2Paddle :: (Float, Float)	-- right player paddle height
	, player1Up :: Bool
	, player1Down :: Bool
	, player2Up :: Bool
	, player2Down :: Bool
	} deriving Show


-- window stats
width, height, offset :: Int
width = 800
height = 600
offset = 100

-- paddleStats
widthPaddle, heightPaddle, widthField, heightField:: Float
widthPaddle = 20
heightPaddle = 80
widthField = 400
heightField = 300

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
	, player1Up = False
	, player1Down = False
	, player2Up = False
	, player2Down = False
	}

yOffset :: Float
yOffset = 2

inBox :: Float -> Bool
inBox y = y > 0 && y + heightPaddle <= widthField ||
	  y < 0 && y >= -widthField

movePaddles :: Float -> PongGame -> PongGame
movePaddles seconds game = game { player1Paddle = (x1', y1'), player2Paddle = (x2', y2') }
  where
    -- Old locations
    (x1, y1) = player1Paddle game
    (x2, y2) = player2Paddle game
    -- New locations
    x1' = x1
    y1' = if (player1Up game == True) then if (inBox (y1 + yOffset)) then (y1 + yOffset) else y1 else if (player1Down game == True) then if (inBox (y1 - yOffset)) then	(y1 - yOffset) else y1 else y1
    x2' = x2
    y2' = if (player2Up game == True) then y2+5 else if (player2Down game == True) then y2-5 else y2

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
		height = heightPaddle / 2
 		collision = xb - w <= xp && yb <= (yp + height) && yb >= (yp - height)

paddleLeftCollision :: Position -> Position -> Width -> Bool
paddleLeftCollision (xb, yb) (xp, yp) w = collision
 	where
		height = heightPaddle / 2
 		collision = xb + w >= xp && yb <= (yp + height) && yb >= (yp - height)

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
keyboardFunction (EventKey (SpecialKey KeySpace) _ _ _) game = game { player1Paddle = (385, 0), player2Paddle = ((-385), 0), ballLocation = (0, 0) }

keyboardFunction (EventKey (SpecialKey KeyUp) _ _ _) game = if (player1Up game == False) then game { player1Up = True, player1Down = False } else game { player1Up = False } --game { player1Paddle = (vx, vy') }
	where
		(vx, vy) = player1Paddle game
		vy' = if (inBox (vy + yOffset)) then vy + yOffset else vy

keyboardFunction (EventKey (SpecialKey KeyDown) _ _ _) game = if (player1Down game == False) then game { player1Down = True, player1Up = False } else game { player1Down = False } --game { player1Paddle = (vx, vy') }
	where
		(vx, vy) = player1Paddle game
		vy' = vy - yOffset

keyboardFunction (EventKey (Char 'w') _ _ _) game = if (player2Up game == False) then game { player2Up = True, player2Down = False } else game { player2Up = False } --game { player2Paddle = (vx, vy') }
	where
		(vx, vy) = player2Paddle game
		vy' = vy + yOffset

keyboardFunction (EventKey (Char 's') _ _ _) game = if (player2Down game == False) then game { player2Down = True, player2Up = False } else game { player2Down = False } --game { player2Paddle = (vx, vy') }
	where
		(vx, vy) = player2Paddle game
		vy' = vy - yOffset
keyboardFunction _ game = game
