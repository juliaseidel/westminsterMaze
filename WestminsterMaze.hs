module Main where

import System.IO

-- infinite data structure
data Westminster = Junction String Westminster Westminster Westminster
                    | WestminsterBridge
            deriving Show

-- I create a maze that is recursively defined.
westminsterMaze = entrance
  where
    entrance            = Junction "Westminster Catedral. The Westminster Maze is triangle-shaped. Your destination is Westminster Bridge. You can't walk out of Westminster" tateBritain out buckinghamPalace
    buckinghamPalace    = Junction "Buckingham Palace" entrance out bigBen
    bigBen              = Junction "Big Ben" buckinghamPalace WestminsterBridge palaceOfWestminster
    palaceOfWestminster = Junction "Palace of Westminster" bigBen out tateBritain
    tateBritain         = Junction "Tate Britain" palaceOfWestminster out entrance
    out                 = Junction "out of Westminster" out out out 

-- moveLeft, moveRight and moveForward return a maze that results from a move
-- press r to move right, press l to move left or press f to move forward
moveLeft, moveRight, moveForward :: Westminster -> Westminster
moveLeft WestminsterBridge                     = error "Can't move left from Westminster Bridge"
moveLeft  (Junction here left forward right)   = left
moveForward WestminsterBridge                  = error "Can't move forward from Westminster Bridge"
moveForward (Junction here left forward right) = forward
moveRight WestminsterBridge                    = error "Can't move right from Westminster Bridge"
moveRight (Junction here left forward right)   = right

-- I can print out my current location
showLoc :: Westminster -> String
showLoc WestminsterBridge = "You are on Westminster Bridge, you reached your destination."
showLoc (Junction here left forward right) =
   "You are at the " ++ here ++ "."

isExit :: Westminster -> Bool
isExit WestminsterBridge = True
isExit _                 = False

-- wander displays current location and checks for exit.
-- If not exit then it can ask user where to move in maze

wander :: Westminster -> IO()
wander m = 
  do 
     putStrLn (showLoc m)
     if isExit m  
       then return ()
       else moveInWestminster m


-- moveInWestminster prompts user for a character and takes a new portion of 
-- the infinite westminster maze data structure accordingly. Then it calls wander 
-- again with the new westminster maze

moveInWestminster :: Westminster -> IO()
moveInWestminster m =
  do      
     c <- getChar
     putStrLn ""
     let newM 
           | c == 'l'  = moveLeft m
           | c == 'r'  = moveRight m 
           | c == 'f'  = moveForward m
           | otherwise = m
     wander newM

main :: IO()
main =
  do
    hSetBuffering stdin NoBuffering
    wander westminsterMaze