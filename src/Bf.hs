module Bf
  ( BfCommand(..),
    bfPareser,
    syntaxValid,
  )
where

import Data.Maybe

data BfCommand
  = GoRight      -- >
  | GoLeft       -- <
  | Inc          -- +(++) <$> takeWhile (/=']') <*> tail . dropWhile (/=']')
  | Dec          -- -
  | Print        -- .
  | Read         -- ,
  | LoopLeft     -- [
  | LoopRight    -- ]
  deriving (Show, Eq)

type BfSource = [BfCommand]

-- we don't need comment. So when parsing, we just drop it
bfPareser :: String -> BfSource
bfPareser src = if syntaxValid src
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

syntaxValid :: String -> Bool
syntaxValid =
    isValid . filter (\x -> x=='[' || x== ']')
    where
      isValid [] = True
      isValid ('[':xs) =
        not (null xs) &&
        isValid ((++) <$> takeWhile (/=']') <*> tail . dropWhile (/=']') $ xs)
      isValid _ = False