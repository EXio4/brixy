module Brixy.Compiler (defSettings, compile) where

import           Brixy.AST
import qualified Brainfuck.Types as BF

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Int
import           Data.Word

import           Control.Monad.Trans.State
import           Control.Monad


{-
    the basic parts of the compiler are:
        * basic verification [TODO]
                makes sure all variables in the program are declared earlier / available in the scope
                [TODO: check variable arguments are given properly]
                [TODO: what else should be checked?]
        * the early allocator
                finds all global variables and initializes their values, spots the main function
        * compileFuncall [main]
                the main function is compiled [funcalls revised]
        * funcalls are compiled by getting inlined, they get a chunk of "memory" that they "leave as garbage" later

        [the whole thing is compiled to a higher-level brainfuck, and it's slowly reduced in complexity until we're left on with normal brainfuck]
-}

{- L1 [high-level BF] -}
data L1BF = L1ValInc   !Int64 !Word8
          | L1IOOutput !Int64
          | L1IORead   !Int64
          | L1While    !Int64 [L1BF]
          | L1Set      !Int64 !Word8
          | L1Copy     !Int64 !Int64
          | L1Move     !Int64 !Int64
    deriving (Show)

data L2BF = L2ValInc   !Int64 !Word8
          | L2IOOutput !Int64
          | L2IORead   !Int64
          | L2While    !Int64 [L2BF]
    deriving (Show)


l2_to_bf :: [L2BF] -> BF.Program
l2_to_bf = flip evalState 0 . go where
    go prog = concat <$> mapM f prog
    f (L2ValInc i w) = (++) <$> move i <*> pure [BF.ValInc w]
    f (L2IOOutput i) = (++) <$> move i <*> pure [BF.IOOutput]
    f (L2IORead   i) = (++) <$> move i <*> pure [BF.IORead]
    f (L2While i cs) = do cond1 <- move i
                          body  <- go cs
                          cond2 <- move i
                          return (cond1 ++ [BF.While (body ++ cond2)])
    move :: Int64 -> State Int64 [BF.BF]
    move n = do curr <- get
                put n
                return $ if n >= curr
                         then [BF.PtrInc (n - curr)]
                         else [BF.PtrInc (curr - n)]

l1_to_l2 :: [L1BF] -> [L2BF]
l1_to_l2 = concatMap comp where
        local0 = 0
        comp (L1ValInc a x) = [L2ValInc (a+16) x]
        comp (L1IOOutput a) = [L2IOOutput (a+16)]
        comp (L1IORead   a) = [L2IORead (a+16)]
        comp (L1While a xs) = [L2While (a+16) (l1_to_l2 xs)]
        comp (L1Set   a nw) = [L2While (a+16) [L2ValInc (a+16) (-1)]
                              ,L2ValInc (a+16) nw]
        comp (L1Move  a b ) = [L2While (a+16) [L2ValInc (a+16) (-1)]
                              ,L2While (b+16) [L2ValInc (b+16) (-1)
                                              ,L2ValInc (a+16)   1]]
        comp (L1Copy  a b ) = [L2While (a+16) [L2ValInc (a+16) (-1)]
                              ,L2While local0 [L2ValInc local0 (-1)]
                              ,L2While (b+16) [L2ValInc (b+16) (-1)
                                              ,L2ValInc (a+16)   1
                                              ,L2ValInc local0   1]
                              ,L2While local0 [L2ValInc local0 (-1)
                                              ,L2ValInc (b+16)   1]]

data CompilerSettings = CompilerSettings {

} deriving (Show)

defSettings = CompilerSettings

data FunctionDef = FunctionDef [String] [Statement]

data CompilerError = VariableNotFound {- `stack` trace -} [Ident] !Ident
                   | FuckedUp
            deriving (Show)

compile :: CompilerSettings -> Program -> Either CompilerError BF.Program
compile _ p = Right (flip evalState (CK [] M.empty) $ do
                                genCompilerStack p
                                mainRes <- dk_declare "#main_res"
                                main    <- dk_lookup_fun "main"
                                (l2_to_bf . l1_to_l2) <$> compileFuncall mainRes main [])

{- variables starting with # are reversed for compiler internals -}

genCompilerStack :: Program -> State CompilerStack ()
genCompilerStack (Module _ defs) = do dk_enter "global_table"
                                      forM_ defs $ \x -> case x of
                                          Declaration (Ident n) -> dk_declare n >> return ()
                                          Function (Ident xid) params stms -> dk_declare_fun xid (map (\(Ident i) -> i) params) stms

data CompilerStack = CK {
     ck_curr_stack :: [(String,M.Map String Int64)] {- every element represents a block + the name of it -}
    ,ck_funs       :: M.Map String FunctionDef
}

{- main operations done on the CS
    * enter a new scope
    * leave a scope [and thus "free" the stack variables]
    * lookup a variable
    * declare a variable
-}
dk_enter :: String -> State CompilerStack ()
dk_enter snam = modify (\(CK xs funs) -> CK ((snam,M.empty) : xs) funs)

{- NOTE: if we're at the "top-level" scope, we simply do nothing -}
dk_leave :: State CompilerStack ()
dk_leave = modify (\(CK xs funs) -> CK (case xs of [x] -> [x] ; (_ : xs) -> xs) funs)

dk_lookup :: String -> State CompilerStack Int64
dk_lookup xid = do CK xs _ <- get
                   let go [] = error $ "Variable not found, should never happen \nCalltrace: " ++ show (map fst xs)
                       go ((_,m):ms) = case M.lookup xid m of
                                        Nothing -> go ms
                                        Just p  -> p
                   return (go xs)

dk_declare_fun :: String -> [String] -> [Statement] -> State CompilerStack ()
dk_declare_fun str params stms = modify (\(CK xs funs) ->
                                                CK xs (M.insert str (FunctionDef params stms) funs))

dk_declare :: String -> State CompilerStack Int64
dk_declare xid = modify (\(CK xs funs) ->
                            CK (f xs) funs) >> dk_lookup xid
    where f q@((s, m) : ms) = (s , M.insert xid (getCurrMax q + 1) m) : ms
          getCurrMax :: [(String, M.Map String Int64)] -> Int64
          getCurrMax [] = 16 {- magic number, should be obtained from CompilerSettings -}
          getCurrMax ((_, m):xs) = case M.elems m of
                                        [] -> getCurrMax xs
                                        mm -> maximum mm


dk_lookup_fun :: String -> State CompilerStack FunctionDef
dk_lookup_fun xid = do CK _ funs <- get
                       case M.lookup xid funs of
                            Nothing -> error $ "Function should be around here (" ++ xid ++ ")"
                            Just  v -> return v


compileExpr :: Int64 -> Expr -> State CompilerStack [L1BF]
compileExpr resAddr (VL (Ident name)) = do i <- dk_lookup name
                                           return [L1Copy resAddr i]
compileExpr resAddr (Lit w)           = do return [L1Set  resAddr w]
compileExpr resAddr (Add e1 e2)       = do dk_enter "#addition"
                                           i2 <- dk_declare "#e2"
                                           v1 <- compileExpr resAddr e1
                                           v2 <- compileExpr i2 e2
                                           dk_leave
                                           return (
                                            [L1Set resAddr 0
                                            ,L1Set i2      0] ++ v1 ++ v2 ++
                                            [L1While i2
                                                [L1ValInc i2 (-1)
                                                ,L1ValInc resAddr 1
                                             ]
                                            ])
compileExpr resAddr (Minus e1 e2)     = do dk_enter "#substraction"
                                           i2 <- dk_declare "#e2"
                                           v1 <- compileExpr resAddr e1
                                           v2 <- compileExpr i2 e2
                                           dk_leave
                                           return (
                                            [L1Set resAddr 0
                                            ,L1Set i2      0] ++ v1 ++ v2 ++
                                            [L1While i2
                                                [L1ValInc i2 (-1)
                                                ,L1ValInc resAddr (-1)
                                                ]
                                            ])
compileExpr resAddr (Equal e1 e2)     = do dk_enter "#equal"
                                           i2 <- dk_declare "#e2"
                                           v1 <- compileExpr resAddr e1
                                           v2 <- compileExpr i2 e2
                                           dk_leave
                                           return (
                                            [L1Set resAddr 0
                                            ,L1Set i2      0] ++ v1 ++ v2 ++
                                            [L1While resAddr
                                                [L1ValInc resAddr (-1)
                                                ,L1ValInc i2      (-1)
                                                ]
                                            ,L1ValInc resAddr 1
                                            ,L1While i2
                                                [L1ValInc resAddr (-1)
                                                ,L1Set    i2      0
                                                ]
                                            ])

compileExpr resAddr (CallF (Ident n) exps) =  do dk_enter ("#funcall_expr_" ++ n)
                                                 xs <- forM (zip [1..] exps) $ \(n,ex) -> do
                                                                    i_n <- dk_declare ("#i_" ++ show n)
                                                                    xcd <- compileExpr i_n ex
                                                                    return (L1Set i_n 0 : xcd, i_n)
                                                 fun <- dk_lookup_fun n
                                                 code <- compileFuncall resAddr fun (map snd xs)
                                                 dk_leave
                                                 return (
                                                    [L1Set resAddr 0
                                                    ] ++ concatMap fst xs ++ code
                                                    )

compileFuncall :: Int64 -> FunctionDef -> [Int64] -> State CompilerStack [L1BF]
compileFuncall resAddr (FunctionDef params stms) paramsAddrs = do
    dk_enter "#funcall"
    initCode <- forM (zip params paramsAddrs) $ \(p,dr) -> do
                                    new_addr <- dk_declare p
                                    return (L1Copy new_addr dr)
    mainCode <- compileStatements resAddr stms
    dk_leave
    return (initCode ++ mainCode)

{-


data Statement = CallE  !Expr
               | Decl   !Ident
               | Assign !Ident !Expr
               | While  !Expr [Statement]
               | IfThenElse !Expr [Statement] [Statement]
               | Print !Expr
               | Return !Expr

-}
compileStatements :: Int64 -> [Statement] -> State CompilerStack [L1BF]
compileStatements resAddr [] = return []
compileStatements resAddr (x:xs) =
    case x of
         Return expr -> compileExpr resAddr expr
         CallE  expr -> (++) <$> compileExpr resAddr expr <*> compileStatements resAddr xs
         Decl   (Ident n) -> do dk_declare n
                                compileStatements resAddr xs
         Assign (Ident n) expr -> do ir <- dk_lookup n
                                     (++) <$> compileExpr ir expr <*> compileStatements resAddr xs
         Print  expr -> (\xs ys -> xs ++ [L1IOOutput resAddr] ++ ys) <$> compileExpr resAddr expr <*> compileStatements resAddr xs
         While expr stms -> do dk_enter "#while"
                               cnd <- dk_declare "#while_cond"
                               condCode <- compileExpr cnd expr
                               bodyCode <- compileStatements resAddr stms
                               dk_leave
                               (\rest -> condCode ++ [L1While cnd (bodyCode ++ condCode)] ++ rest) <$> compileStatements resAddr xs
         IfThenElse cond true false -> do dk_enter "#if_then_else"
                                          condE <- dk_declare "#cond_e"
                                          temp0 <- dk_declare "#temp0"
                                          temp1 <- dk_declare "#temp1"
                                          condCode <- compileExpr condE cond
                                          trueCode <- compileStatements resAddr true
                                          falseCode <- compileStatements resAddr false
                                          dk_leave
                                          return (
                                            [L1Set condE 0
                                            ,L1Set temp0 1
                                            ,L1Set temp1 0
                                            ] ++ condCode ++
                                            [L1While condE
                                                (trueCode ++
                                                [L1ValInc temp0 (-1)
                                                ])
                                            ,L1While temp0
                                                (falseCode ++
                                                [L1ValInc temp0 (-1)
                                                ])
                                            ])
