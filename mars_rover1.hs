-- file: MarsRover.hs

{--
	Type definitions related to Plateau
--}

type Position = (Int, Int)

data Plateau = Mars { 
					bottomLeft :: Position, 
					topRight :: Position
				}
				deriving (Eq,Show)


{-- 
	Type definitions related to Board\Surface
--}

data Compass = NORTH | EAST | WEST | SOUTH
				deriving (Eq,Show)

data Move = RIGHT | LEFT | MOVE
				deriving (Eq,Show)

type Orientation = (Compass,Position)

----------------------------------------------------------------------------------------------------				

data Rover = MyRover {
					board :: Plateau,
					orientation :: Orientation,
					commands :: String
				}
				deriving (Eq,Show)

----------------------------------------------------------------------------------------------------

{--
	Possible Commands to Rover
--}				
command :: Char -> Maybe Move
command cmd  = case cmd of
	'l' -> Just LEFT
	'L' -> Just LEFT
	'r' -> Just RIGHT
	'R' -> Just RIGHT
	'm' -> Just MOVE
	'M' -> Just MOVE
	_	-> Nothing

------------------------------------------------------------------------------------------------------	
move :: (Num n1, Num n2) => (Compass, (n1,n2)) -> (Maybe Move) -> (Compass, (n1,n2))
--When Direction is NORTHwards
move (NORTH,(x,y)) (Just LEFT) = (WEST,(x,y))
move (NORTH,(x,y)) (Just RIGHT) = (EAST,(x,y))
move (NORTH,(x,y)) (Just MOVE) = (NORTH,(x,y+1))
--When Direction is SOUTHwards
move (SOUTH,(x,y)) (Just LEFT) = (EAST,(x,y))
move (SOUTH,(x,y)) (Just RIGHT) = (WEST,(x,y))
move (SOUTH,(x,y)) (Just MOVE) = (NORTH,(x,y-1))
--When Direction is EASTwards
move (EAST,(x,y)) (Just LEFT) = (NORTH,(x,y))
move (EAST,(x,y)) (Just RIGHT) = (SOUTH,(x,y))
move (EAST,(x,y)) (Just MOVE) = (EAST,(x+1,y))
--When Direction is WESTwards
move (WEST,(x,y)) (Just LEFT) = (SOUTH,(x,y))
move (WEST,(x,y)) (Just RIGHT) = (NORTH,(x,y))
move (WEST,(x,y)) (Just MOVE) = (WEST,(x-1,y))
--When Direction is Undefined or anything else
move (p,q) _ = (p,q)

--boundaryCheck :: board oldOrientation newOrientation
boundaryCheck (Mars (blx,bly) (trx,try)) (_,(px1,py1)) (_,(px2,py2)) 
	| (blx<=px1) && (blx<=px2) && (trx>=px1) && (trx>=px2) && 
	  (bly<=py1) && (bly<=py2) && (try>=py1) && (try>=py2) = True
	| otherwise = False

--safeMove :: board -> currentOrientation	
safeMove board orientation = let
								newOrientation = move orientation (Just MOVE)
								moveIsSafe = boundaryCheck board orientation newOrientation
							in
								if (moveIsSafe == True) then newOrientation
								else orientation


startRover (MyRover board orientation []) = (MyRover board orientation []) 								
startRover (MyRover board orientation (x:xs)) 
	| (command x == Just MOVE) = startRover (MyRover board (safeMove board orientation) (xs))
	| (command x == Just LEFT) || (command x == Just RIGHT) = startRover (MyRover board (move orientation (command x)) (xs))	
	| (command x == Nothing) = (MyRover board orientation (x:xs))
	
{--
	All test data are put below for testing purpose...
--}
mars = Mars (0,0) (5,5)
orientation1 = (NORTH, (1,1))
commands1 = "MMLMML"
rover1 = MyRover mars orientation1 commands1				