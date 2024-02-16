data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Enum)

data Robot = Robot {
               bearing :: Bearing,
               x :: Integer,
               y :: Integer
                   }

coordinates :: Robot -> (Integer, Integer)
coordinates robot = (x robot, y robot) 

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = Robot direction (fst coordinates) (snd coordinates)

-- | The function uses recursion to process each instruction in the string.
move :: Robot -> String -> Robot
-- Base case: If there are no more instructions, the robot's current state is returned.
move robot [] = robot
-- Recursive case: The first instruction in the string is processed, and the function is called recursively with the rest of the string.
move robot (z:zs) = move (doInstruction z) zs
    where 
        -- The 'doInstruction' function processes a single instruction.
        doInstruction 'A' = advance robot -- If the instruction is 'A', the robot advances.
        doInstruction i = turn i robot -- If the instruction is 'R' or 'L', the robot turns. Any other instruction is ignored.
        -- The 'advance' function moves the robot one step in the direction it is currently facing.
        advance (Robot North x' y') = mkRobot North (x', (y'+1)) 
        advance (Robot East x' y') = mkRobot East   ((x'+1), y')
        advance (Robot South x' y') = mkRobot South (x', (y'-1))
        advance (Robot West x' y') = mkRobot West   ((x'-1), y')
        -- The 'turn' function changes the direction the robot is facing.
        turn 'R' (Robot direc x' y') = mkRobot (toEnum . (flip mod 4) $ fromEnum direc + 1) (x', y') -- If the instruction is 'R', the robot turns right.
        turn 'L' (Robot direc x' y') = mkRobot (toEnum . (flip mod 4) $ fromEnum direc - 1) (x', y') -- If the instruction is 'L', the robot turns left.
        turn _ _ = robot -- Any other instruction is ignored.
 
