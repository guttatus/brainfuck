module Bf
  ( BfCommand(..),
    bfPareser,
  )
where

import Data.Maybe

data BfCommand
  = GoRight      -- >
  | GoLeft       -- <
  | Inc          -- +
  | Dec          -- -
  | Print        -- .
  | Read         -- ,
  | LoopLeft     -- [
  | LoopRight    -- ]
  deriving (Show, Eq)

type BfSource = [BfCommand]

-- we don't need comment. So when parsing, we just drop it
bfPareser :: String -> BfSource
bfPareser = mapMaybe charToCommand
  where
    charToCommand x
      | x == '>'  = Just GoRight
      | x == '<'  = Just GoLeft
      | x == '+'  = Just Inc
      | x == '-'  = Just Dec
      | x == '.'  = Just Print
      | x == ','  = Just Read
      | x == '['  = Just LoopLeft
      | x == ']'  = Just LoopRight
      | otherwise = Nothing