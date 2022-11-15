module Main (main) where

import Eval(bfbf)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    dispatch args


validArgs :: [String] -> Bool
validArgs args = not (null args || length args /= 1 )

dispatch :: [String] -> IO()
dispatch args = 
    if validArgs args 
    then readFile (head args) >>= bfbf 
    else error "\ESC[31m Error: \ESC[0m invalid arguments"