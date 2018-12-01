module Logic.Chomp where

import Control.Monad.State

-- Ex5. given positive integers m and n, interactively play the game of Chomp

-- type alias and data definition
type Position = (Int, Int)
type Moves = [Position]

poisoned = (0, 0)


-- determine whether a move is illegal
illegalMove :: Moves -> Position -> Bool
illegalMove moves pos = foldr judge True moves
  where judge cur acc = not (cur `inRange` pos) && acc

inRange :: Position -> Position -> Bool
inRange (curX, curY) (x, y) = curX >= x && curY >= y

-- make some move and change global state
data GameResult = InGoing | End

makeMove :: StateT Moves IO GameResult
makeMove = undefined

-- chomp :: Int -> Int -> IO ()
-- chomp m n = undefined