module Brixy.AST where

import Data.Int
import Data.Word

type ModuleName = String

data Program = Module !ModuleName [Decl]
    deriving (Show)

data Param = ByValue !Ident
           | ByRef   !Ident
    deriving (Show)

data Decl = Declaration !Ident
          | Function    !Ident [Param] [Statement]
    deriving (Show)
          {- ^ they're basically macros which get inlined on every call -}

data Ident = Ident !String
    deriving (Show)

data Statement = CallE  !Expr
               | EBF    !EBF
               | Decl   !Ident
               | Assign !Ident !Expr
               | While  !Expr [Statement]
               | IfThenElse !Expr [Statement] [Statement]
               | Return !Expr
    deriving (Show)

data Expr = VL !Ident {- var lookup -}
          | Lit !Word8
          | CallF !Ident ![Expr]
    deriving (Show)

{- L1-based high-level BF -}
data EBF = EBValInc   !EPtr !Word8
         | EBIOOutput !EPtr
         | EBIORead   !EPtr
         | EBWhile    !EPtr [EBF]
         | EBSet      !EPtr !Word8
         | EBCopy     !EPtr !EPtr
         | EBMove     !EPtr !EPtr
         | EBLowLevel [ELLBF]
    deriving (Show)

data EPtr = EAbs   !Int64
          | EIndir !Ident
    deriving (Show)

data EPtrX = EAbsX   !Int64
           | EIndirX !Ident
           | EEndX
    deriving (Show)

data ELLBF = ELLValInc !Word8
           | ELLIOOutput
           | ELLIORead
           | ELLWhile [ELLBF]
           | ELLMove  !EPtrX
           | ELLPtrInc !Int64
    deriving (Show)

prettyPrint :: Program -> String
prettyPrint = (`go1` []) where
    smap :: (a -> (x -> x)) -> [a] -> (x -> x)
    smap f = foldr (.) id . map f
    k = (++)
    goh s [] = k ""
    goh s [x] = k (s x)
    goh s (x : xs) = k (s x) . k ", " . goh s xs

    go1 (Module name decls) = k "module " . k name . k "\n\n" . smap go2 decls
    go2 (Declaration (Ident name)) = k "var " . k name . k ";\n\n"
    go2 (Function (Ident name) params stms)
                        = k "function " . k name . k "(" . goh show params
                                                 . k ")"
                                                 . gos 1 stms . k ";\n\n"

    gos n stms = k "{\n" . smap (\s -> (replicate (4*n) ' '++) . go3 n s . k ";\n") stms
                 . (replicate (4 * (n-1)) ' ' ++) . k "}"

    go3 _ (CallE ex) = go4 ex
    go3 _ (EBF{}) = k "<internal bf code>"
    go3 _ (Decl  (Ident name)) = k "var " . k name
    go3 _ (Assign (Ident name) expr) = k name . k " := " . go4 expr
    go3 n (While  expr stms) = k "while (" . go4 expr . k ") " . gos (n+1) stms
    go3 n (IfThenElse cond true false) = k "if (" . go4 cond . k ") " . gos (n+1) true . k " else " . gos (n+1) false
    go3 _ (Return expr) = k "return#(" . go4 expr . k ")"
    
    go4 (VL (Ident name)) = k name
    go4 (Lit w) = k (show w)
    go4 (CallF (Ident f) exprs) = k f . k "(" . goh (`go4`[]) exprs . k ")"
