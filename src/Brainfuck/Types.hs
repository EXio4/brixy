module Brainfuck.Types (Program, BF(..), showAsProgram) where

import Data.Int
import Data.Word

data BF = PtrInc !Int64
        | ValInc !Word8
        | IOOutput
        | IORead
        | While [BF]
    deriving (Show)

type Program = [BF]


showAsProgram :: Program -> String
showAsProgram = (`sh` []) where
    sh = foldr (.) id . map go
    go (PtrInc n)  | n >  0 = (replicate (fromIntegral   n)  '>' ++)
    go (PtrInc n)  | n <= 0 = (replicate (fromIntegral (-n)) '<' ++)
    go (ValInc n)  | n >  128 = (replicate (256 - fromIntegral n) '-' ++)
    go (ValInc n)  | n <= 128 = (replicate (fromIntegral n)       '+' ++)
    go IOOutput    = ('.':)
    go IORead      = (',':)
    go (While prog)  = ('[':) . sh prog . (']':)
