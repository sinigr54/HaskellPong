module Game where

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
	, ballVelocity = (88, -166)
	, player1Paddle = 0
	, player2Paddle = 0
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

paddleCollision :: Position -> Position -> Width -> Bool
paddleCollision (xb, yb) (xp, yp) w = collisionL || collisionR
 	where
 		collisionL = xb -
		collisionR =

-- Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce = undefined

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
