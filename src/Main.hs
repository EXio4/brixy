module Main where


import Data.Char


import qualified Brainfuck.Types as BF
import Brainfuck.Interpreter

import Brixy.AST
import Brixy.Compiler
{-
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
        
prog4 =
    Module "Main"
        [Function (Ident "add") [Ident "i1", Ident "i2"]
            [EBF (EBWhile (EIndir (Ident "i2"))
                    [EBValInc (EIndir (Ident "i2")) (-1)
                    ,EBValInc (EIndir (Ident "i1")) 1
                    ])
            ,Return (VL (Ident "i1"))
            ]
        ,Function (Ident "main") []
            [Print (CallF (Ident "add") [Lit 2, Lit 3])
            ]
        ]
        -}
prog5 =
    Module "Main"
        [Function (Ident "not") [ByValue (Ident "x")]
            [IfThenElse (CallF (Ident "equal") [VL (Ident "x"),Lit 0])
                [Return (Lit 1)]
                [Return (Lit 0)]]
        ,Function (Ident "add") [ByValue (Ident "i1"), ByValue (Ident "i2")]
            [EBF (EBWhile (EIndir (Ident "i2"))
                    [EBValInc (EIndir (Ident "i2")) (-1)
                    ,EBValInc (EIndir (Ident "i1")) 1
                    ])
            ,Return (VL (Ident "i1"))
            ]
        ,Function (Ident "minus") [ByValue (Ident "i1"), ByValue (Ident "i2")]
            [EBF (EBWhile (EIndir (Ident "i2"))
                    [EBValInc (EIndir (Ident "i2")) (-1)
                    ,EBValInc (EIndir (Ident "i1")) (-1)
                    ])
            ,Return (VL (Ident "i1"))
            ]
        ,Function (Ident "equal") [ByValue (Ident "i1"), ByValue (Ident "i2")]
            [EBF (EBWhile (EIndir (Ident "i1"))
                    [EBValInc (EIndir (Ident "i1")) (-1)
                    ,EBValInc (EIndir (Ident "i2")) (-1)
                    ])
            ,EBF (EBValInc (EIndir (Ident "i1")) 1)
            ,EBF (EBWhile (EIndir (Ident "i2"))
                    [EBValInc (EIndir (Ident "i1")) (-1)
                    ,EBSet    (EIndir (Ident "i2")) 0
                    ])
            ,Return (VL (Ident "i1"))
            ]
        ,Function (Ident "mult") [ByValue (Ident "x"), ByValue (Ident "y")]
            [Decl (Ident "acc")
            ,Assign (Ident "acc") (Lit 0)
            ,While (CallF (Ident "not") [CallF (Ident "equal") [VL (Ident "y"),Lit 0]])
                [Assign (Ident "acc") (CallF (Ident "add") [VL (Ident "acc"),VL (Ident "x")])
                ,Assign (Ident "y")   (CallF (Ident "add") [VL (Ident "y"),Lit (-1)])]
            ,Return (VL (Ident "acc"))]
        ,Function (Ident "print") [ByValue (Ident "x")]
            [EBF (EBIOOutput (EIndir (Ident "x")))
            ,Return (Lit 0)
            ]
        ,Function (Ident "read") []
            [Decl (Ident "ret")
            ,EBF (EBIORead (EIndir (Ident "ret")))
            ,Return (VL (Ident "ret"))
            ]
        ,Function (Ident "factorial") [ByValue (Ident "x")]
            [Decl (Ident "acc")
            ,Assign (Ident "acc") (Lit 1)
            ,While (CallF (Ident "not") [CallF (Ident "equal") [VL (Ident "x"),Lit 0]])
                [
                     Assign (Ident "acc") (CallF (Ident "mult") [(VL (Ident "acc")),(VL (Ident "x"))])
                    ,Assign (Ident "x"  ) (CallF (Ident "minus") [VL (Ident "x"),Lit 1])
                ]
            ,CallE (CallF (Ident "add48") [VL (Ident "acc")])
            ,CallE (CallF (Ident "print") [VL (Ident "acc")])
            ]
        ,Function (Ident "add48") [ByRef (Ident "x")]
            [Assign (Ident "x") (CallF (Ident "add") [VL (Ident "x"), Lit 48])
            ,Return (Lit 0)
            ]
        ,Function (Ident "main") []
            [Decl (Ident "x")
            ,Assign (Ident "x") (CallF (Ident "read") [])
            ,CallE (CallF (Ident "factorial") [VL (Ident "x")])
            ]
        ]
{-
compileExpr resAddr (Add e1 e2)       = do dk_enter "#addition"
                                           i1 <- dk_declare "#e1"
                                           i2 <- dk_declare "#e2"
                                           v1 <- compileExpr i1 e1
                                           v2 <- compileExpr i2 e2
                                           dk_leave
                                           return (
                                            [L1Set i1      0
                                            ,L1Set i2      0] ++ v1 ++ v2 ++
                                            [L1While i2
                                                [L1ValInc i2 (-1)
                                                ,L1ValInc i1 1
                                                ]
                                            ,L1Copy resAddr i1
                                            ])
                                            -}

run xs p = ir_stdout $ eval_naive (case compile defSettings p of Right w -> w) xs


gen fn mainBody = case fn defSettings (Module "Main" [Function (Ident "main") [] mainBody]) of
                    Left{} -> error ".."
                    Right p -> p


var = Decl . Ident
(=:) a b = Ident a `Assign` b
v = Ident
main :: IO ()
main = putStrLn "Hello, Haskell!"
