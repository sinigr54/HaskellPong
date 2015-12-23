module Game where

-- state of pong game
data PongGame = Game
	{ ballLocation :: (Float, Float) 	-- x, y of the pong ball location
	, ballVelocity :: (Float, Float) 	-- x, y of the pong ball velocity
	, player1 :: Float				 	-- left player paddle height
										-- Zero is middle of screen
	, player2 :: Float					-- right player paddle height	
	} deriving Show
	

	
--init game with starting state
initialState :: PongGame
initialState = Game
	{ ballLocation = (-10, 30)
	, ballVelocity = (1, -3)
	, player1 = 40
	, player2 = -80
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