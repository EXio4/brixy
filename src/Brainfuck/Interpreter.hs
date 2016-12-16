module Brainfuck.Interpreter (eval_naive,InterpreterResult(..)) where

import           Control.Monad.Trans.State
import           Brainfuck.Types
import           Data.Word
import           Data.Int


data InterpreterResult = InterpreterResult {
            ir_stdout :: [Word8]
           ,ir_tape   :: ([Word8], Word8, [Word8])
} deriving (Show)

eval_naive :: Program -> [Word8] -> InterpreterResult
eval_naive prog input = snd $ execState (mapM_ go prog) (input, InterpreterResult [] (repeat 0, 0, repeat 0))

go :: BF -> State ([Word8], InterpreterResult) ()
go (ValInc inc) = modify (\(inp, InterpreterResult stdout (back, curr, next)) ->
                                (inp, InterpreterResult stdout (back, curr + inc, next)))
go (PtrInc off) = modify (\(inp, InterpreterResult stdout tape) ->
                                (inp, InterpreterResult stdout (move (fromIntegral off) tape)))
go IOOutput     = modify (\(inp, InterpreterResult stdout (back, curr, next)) ->
                                (inp, InterpreterResult (curr : stdout) (back, curr, next)))
go IORead       = modify (\(inp, InterpreterResult stdout (back, curr, next)) ->
                                case inp of
                                     []       -> ([], InterpreterResult stdout (back, 0, next))
                                     (x : xs) -> (xs, InterpreterResult stdout (back, x, next)))
go (While prog) = do (_, InterpreterResult _ (_, curr, _)) <- get
                     if curr == 0
                     then return ()
                     else mapM_ go prog >> go (While prog)

move :: Int -> ([Word8], Word8, [Word8]) -> ([Word8], Word8, [Word8])
move off (back, curr, next)
    | off > 0 = let (new_curr:new_next) = drop off (curr : next)
                    new_back = take off (curr : next)
                in (reverse new_back ++ back, new_curr, new_next)
    | off == 0 = (back , curr, next)
    | off < 0 = let (a,b,c) = move (-off) (next, curr, back)
                in (c,b,a)





test_bf_prog =
       [ValInc 10
       ,While [
            ValInc (-1)
           ,PtrInc 1
           ,ValInc 1
           ,PtrInc 1
           ,ValInc 5
           ,PtrInc (-2)
        ]
       ]
