module Eval
  (bfbf
  )
where

import Data.Char ( ord, chr )
import Parser (BfCommand (..), BfSource, bfPareser)
import System.IO ( hFlush, stdout )
import Tape (Tape (..), emptyTape, moveLeft, moveRight)

bfbf :: String -> IO()
bfbf = bfInit . bfPareser

bfInit :: BfSource -> IO ()
bfInit = bfRun emptyTape . bfSourceToTape
  where
    bfSourceToTape (s : ss) = Tape [] s ss

bfRun :: Tape Int -> Tape BfCommand -> IO ()
bfRun dataTape cmdTape@(Tape _ GoRight _) =
  bfNextCmd (moveRight dataTape) cmdTape
bfRun dataTape cmdTape@(Tape _ GoLeft _) =
  bfNextCmd (moveLeft dataTape) cmdTape
bfRun (Tape ls p rs) cmdTape@(Tape _ Inc _) =
  bfNextCmd (Tape ls (p + 1) rs) cmdTape
bfRun (Tape ls p rs) cmdTape@(Tape _ Dec _) =
  bfNextCmd (Tape ls (p - 1) rs) cmdTape
bfRun dataTape@(Tape _ p _) cmdTape@(Tape _ Print _) = do
  putChar $ chr p
  hFlush stdout
  bfNextCmd dataTape cmdTape
bfRun (Tape ls _ rs) cmdTape@(Tape _ Read _) = do
  p <- getChar
  bfNextCmd (Tape ls (ord p) rs) cmdTape
bfRun dataTape@(Tape _ p _) cmdTape@(Tape _ LoopLeft _)
  | p == 0 = bfSeekLoopR 0 dataTape cmdTape
  | otherwise = bfNextCmd dataTape cmdTape
bfRun dataTape@(Tape _ p _) cmdTape@(Tape _ LoopRight _)
  | p /= 0 = bfSeekLoopL 0 dataTape cmdTape
  | otherwise = bfNextCmd dataTape cmdTape

bfNextCmd :: Tape Int -> Tape BfCommand -> IO ()
bfNextCmd _ (Tape _ _ []) = return () -- end
bfNextCmd dataTape cmdTape = bfRun dataTape (moveRight cmdTape)

bfSeekLoopR :: Int -> Tape Int -> Tape BfCommand -> IO()
bfSeekLoopR 1 dataTape cmdTape@(Tape _ LoopRight _) =
    bfNextCmd dataTape cmdTape
bfSeekLoopR acc dataTape cmdTape@(Tape _ LoopRight _) = 
    bfSeekLoopR (acc-1) dataTape (moveRight cmdTape)
bfSeekLoopR acc dataTape cmdTape@(Tape _ LoopLeft _) = 
    bfSeekLoopR (acc+1) dataTape (moveRight cmdTape)
bfSeekLoopR acc dataTape cmdTape = 
    bfSeekLoopR acc dataTape (moveRight  cmdTape)  

bfSeekLoopL :: Int -> Tape Int -> Tape BfCommand -> IO()
bfSeekLoopL 1 dataTape cmdTape@(Tape _ LoopLeft _) =
    bfNextCmd dataTape cmdTape
bfSeekLoopL acc dataTape cmdTape@(Tape _ LoopLeft _) =
    bfSeekLoopL (acc-1) dataTape (moveLeft cmdTape)
bfSeekLoopL acc dataTape cmdTape@(Tape _ LoopRight _) =
    bfSeekLoopL (acc+1) dataTape (moveLeft cmdTape)
bfSeekLoopL acc dataTape cmdTape =
    bfSeekLoopL acc dataTape (moveLeft cmdTape)