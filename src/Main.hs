module Main where

import Brainfuck.Interpreter

import Brixy.AST
import Brixy.Compiler


test_brixy_prog =
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
            ,While (Equal (VL (Ident "x")) (Lit 0))
                [Assign (Ident "acc") (CallF (Ident "mult") [(VL (Ident "acc")),(VL (Ident "x"))])
                ,Assign (Ident "x"  ) (Minus (VL (Ident "x")) (Lit 1))
                ]
            ,Assign (Ident "acc") (Add (VL (Ident "acc")) (Lit 48))
            ,Print (VL (Ident "acc"))
            ]
        ,Function (Ident "main") []
            [CallE (CallF (Ident "factorial") [Lit 3])
            ,CallE (CallF (Ident "factorial") [Lit 4])
            ]
        ]

main :: IO ()
main = putStrLn "Hello, Haskell!"
