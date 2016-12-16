module Main where


import Data.Char


import qualified Brainfuck.Types as BF
import Brainfuck.Interpreter

import Brixy.AST
import Brixy.Compiler

simpl = Module "Main" 
            [Function (Ident "test") [Ident "x"]
                [Print (VL (Ident "x"))]
            ,Function (Ident "main") [] 
            [CallE (CallF (Ident "test") [Lit 48])]
            ]

prog1 = Module "Main"
            [Function (Ident "not") [Ident "x"]
                [IfThenElse (Equal (VL (Ident "x")) (Lit 0))
                    [Return (Lit 1)]
                    [Return (Lit 0)]
                ]
            ,Function (Ident "main") []
                [Print (CallF (Ident "not") [Lit 0])]]

prog2 = Module "Main"
            [Function (Ident "not") [Ident "x"]
                [IfThenElse (Equal (VL (Ident "x")) (Lit 0))
                    [Return (Lit 1)]
                    [Return (Lit 0)]
                ]
            ,Function (Ident "mult") [Ident "x", Ident "y"]
                [Decl (Ident "acc")
                ,Assign (Ident "acc") (Lit 0)
                ,While (CallF (Ident "not") [Equal (VL (Ident "y")) (Lit 0)])
                    [Assign (Ident "acc") (Add (VL (Ident "acc")) (VL (Ident "x")))
                    ,Assign (Ident "y")   (Add (VL (Ident "y")) (Lit (-1)))]
                ,Return (VL (Ident "acc"))
                ]
            ,Function (Ident "main") []
                [Print (CallF (Ident "mult") [Lit 3, Lit 2])]]

prog3 =
    Module "Main"
        [Function (Ident "not") [Ident "x"]
            [IfThenElse (Equal (VL (Ident "x")) (Lit 0))
                [Return (Lit 1)]
                [Return (Lit 0)]]
        ,Function (Ident "mult") [Ident "x", Ident "y"]
            [Decl (Ident "acc")
            ,Assign (Ident "acc") (Lit 0)
            ,While (CallF (Ident "not") [Equal (VL (Ident "y")) (Lit 0)])
                [Assign (Ident "acc") (Add (VL (Ident "acc")) (VL (Ident "x")))
                ,Assign (Ident "y")   (Add (VL (Ident "y")) (Lit (-1)))]
            ,Return (VL (Ident "acc"))]
        ,Function (Ident "factorial") [Ident "x"]
            [Decl (Ident "acc")
            ,Assign (Ident "acc") (Lit 1)
            ,While (CallF (Ident "not") [Equal (VL (Ident "x")) (Lit 0)])
                [
                     Assign (Ident "acc") (CallF (Ident "mult") [(VL (Ident "acc")),(VL (Ident "x"))])
                    ,Assign (Ident "x"  ) (Minus (VL (Ident "x")) (Lit 1))
                ]
    --        ,Assign (Ident "acc") (Add (VL (Ident "acc")) (Lit 48))
            ,Print (VL (Ident "acc"))
            ]
        ,Function (Ident "main") []
            [CallE (CallF (Ident "factorial") [Lit 3])
            --,CallE (CallF (Ident "factorial") [Lit 4])
            ]
        ]

run p = ir_stdout $ eval_naive (case compile defSettings p of Right w -> w) []


gen fn mainBody = case fn defSettings (Module "Main" [Function (Ident "main") [] mainBody]) of
                    Left{} -> error ".."
                    Right p -> p


var = Decl . Ident
(=:) a b = Ident a `Assign` b
v = Ident
main :: IO ()
main = putStrLn "Hello, Haskell!"
