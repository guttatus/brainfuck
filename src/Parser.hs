module Parser
  ( BfCommand(..),
    BfSource,
    bfPareser,
    bfSyntaxValid,
  )
where

import Data.Maybe ( mapMaybe )

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
bfPareser src = 
  if bfSyntaxValid src
  then mapMaybe charToCommand src
  else error "\ESC[31m Syntax error!!\ESC[0m Please check your code."
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

bfSyntaxValid :: String -> Bool
bfSyntaxValid =
    isValid . filter (\x -> x=='[' || x== ']')
    where
      isValid [] = True
      isValid ('[':xs) =
        not (null xs) &&
        isValid ((++) <$> takeWhile (/=']') <*> tail . dropWhile (/=']') $ xs)
      isValid _ = False