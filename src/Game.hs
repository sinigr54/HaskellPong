module Game where


-- state of pong game
data PongGame = Game
	{ ballLocation :: (Float, Float) 	-- x, y of the pong ball location
	, ballVelocity :: (Float, Float) 	-- x, y of the pong ball velocity
	, player1Paddle :: (Float, Float)	-- left player paddle location
										-- Zero is middle of screen
	, player2Paddle :: (Float, Float)	-- right player paddle height
	-- flags used for paddle movement
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

-- players stats
p1X, p2X, p1Y, p2Y :: Float
p1X = widthField - 15
p2X = -p1X
p1Y = 0
p2Y = 0

-- ball stats
velocityX, velocityY, ballX, ballY :: Float
velocityX = 350
velocityY = -300
ballX = 0
ballY = 0

-- game-related type alliases
type Radius = Float
type Width = Float
type Height = Float
type Position = (Float, Float)

-- init game with starting state
initialState :: PongGame
initialState = Game
	{ ballLocation = (ballX, ballY)
	, ballVelocity = (velocityX, velocityY)
	, player1Paddle = (p1X, p1Y)
	, player2Paddle = (p2X, p2Y)
	, player1Up = False
	, player1Down = False
	, player2Up = False
	, player2Down = False
	}

yOffset :: Float
yOffset = 5

inBox :: Float -> Bool
inBox 0 = True
inBox y = y > 0 && y + heightPaddle / 2 <= heightField ||
	  y < 0 && y - heightPaddle / 2 >= -heightField

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
    y2' = if (player2Up game == True) then if (inBox (y2 + yOffset)) then (y2 + yOffset) else y2 else if (player2Down game == True) then if (inBox (y2 - yOffset)) then	(y2 - yOffset) else y2 else y2

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
 		collision = xb - w <= xp && xb + w >= xp && yb <= (yp + height) && yb >= (yp - height)

paddleLeftCollision :: Position -> Position -> Width -> Bool
paddleLeftCollision (xb, yb) (xp, yp) w = collision
 	where
		height = heightPaddle / 2
 		collision = xb + w >= xp && xb - w <= xp && yb <= (yp + height) && yb >= (yp - height)

-- Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVelocity = (vx', vy') }
	where
		w = widthPaddle
		h = heightPaddle / 4
		(vx, vy) = ballVelocity game

		vx' = if (paddleLeftCollision (ballLocation game) (player1Paddle game) w) && (vx > 0)
			|| (paddleRightCollision (ballLocation game) (player2Paddle game) w) && (vx < 0)
			then
				-vx
			else
				vx

		vy' = if (paddleLeftCollision (ballLocation game) (player1Paddle game) w)
			then
				if( (snd $ ballLocation game) < ( (snd $ player1Paddle game) - h) ) then -- upper part
					increase $ sendUp vy
				else if( (snd $ ballLocation game) > ( (snd $ player1Paddle game) + h) ) then -- lower part
					increase $ sendDown vy
				else reduce vy -- middle part, gravitate ball y velocity towards null, while also inverting it

			else if (paddleRightCollision (ballLocation game) (player2Paddle game) w) then
				if( (snd $ ballLocation game) < ( (snd $ player2Paddle game) - h) ) then -- upper part
					increase $ sendUp vy
				else if( (snd $ ballLocation game) > ( (snd $ player2Paddle game) + h) ) then -- lower part
					increase $ sendDown vy
				else reduce vy -- middle part, gravitate ball y velocity towards null, while also inverting it
			else
				vy

		invert :: Float -> Float
		invert v = -v

		sendUp :: Float -> Float
		sendUp v = if (v > 0) then -v else v

		sendDown :: Float -> Float
		sendDown v = if (v < 0) then -v else v

		reduce :: Float -> Float
		reduce v = if (v > 0)then v - 50 else v + 50

		increase :: Float -> Float
		increase v = if (v > 0)then v + 50 else v - 50

-- Detect a collision with one of the side walls. Upon collisions,
-- change the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVelocity = (vx, vy') }
  where
    -- ball radius, same as in render.
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
