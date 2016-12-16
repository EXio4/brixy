module Brixy.AST where


import Data.Word

type ModuleName = String

data Program = Module !ModuleName [Decl]
    deriving (Show)

data Decl = Declaration !Ident
          | Function    !Ident [Ident] [Statement]
    deriving (Show)
          {- ^ they're basically macros which get inlined on every call -}

data Ident = Ident !String
    deriving (Show)

data Statement = CallE  !Expr
               | Decl   !Ident
               | Assign !Ident !Expr
               | While  !Expr [Statement]
               | IfThenElse !Expr [Statement] [Statement]
               | Print !Expr
               | Return !Expr
    deriving (Show)

data Expr = VL !Ident {- var lookup -}
          | Lit !Word8
          | Add   !Expr !Expr
          | Minus !Expr !Expr
          | Equal !Expr !Expr
          | CallF !Ident ![Expr]
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
                        = k "function " . k name . k "(" . goh (\(Ident i) -> i) params
                                                 . k ")"
                                                 . gos 1 stms . k ";\n\n"

    gos n stms = k "{\n" . smap (\s -> (replicate (4*n) ' '++) . go3 n s . k ";\n") stms
                 . (replicate (4 * (n-1)) ' ' ++) . k "}"

    go3 _ (CallE ex) = go4 ex
    go3 _ (Decl  (Ident name)) = k "var " . k name
    go3 _ (Assign (Ident name) expr) = k name . k " := " . go4 expr
    go3 n (While  expr stms) = k "while (" . go4 expr . k ") " . gos (n+1) stms
    go3 n (IfThenElse cond true false) = k "if (" . go4 cond . k ") " . gos (n+1) true . k " else " . gos (n+1) false
    go3 _ (Print expr) = k "print#(" . go4 expr . k ")"
    go3 _ (Return expr) = k "return#(" . go4 expr . k ")"
    
    go4 (VL (Ident name)) = k name
    go4 (Lit w) = k (show w)
    go4 (Add   e1 e2) = k "(" . go4 e1 . k " + "  . go4 e2 . k ")"
    go4 (Minus e1 e2) = k "(" . go4 e1 . k " - "  . go4 e2 . k ")"
    go4 (Equal e1 e2) = k "(" . go4 e1 . k " == " . go4 e2 . k ")"
    go4 (CallF (Ident f) exprs) = k f . k "(" . goh (`go4`[]) exprs . k ")"
