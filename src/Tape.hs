module Tape 
  ( Tape(..),
    emptyTape,
    moveLeft,
    moveRight,
  )
where

data Tape a = Tape [a] a [a]

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros 
    where 
      zeros = repeat 0

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) p rs) = Tape ls l (p:rs)
moveLeft (Tape [] p rs) = Tape [] p rs

moveRight :: Tape a -> Tape a 
moveRight (Tape ls p (r:rs)) = Tape (p:ls) r rs 
moveRight (Tape ls p [])     = Tape ls p []