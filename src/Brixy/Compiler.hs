{-# LANGUAGE BangPatterns #-}
module Brixy.Compiler (defSettings, compile, compile_complete) where

import           Brixy.AST
import qualified Brainfuck.Types as BF

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Int
import           Data.List
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
          | L1LowLevel [L3BF]
    deriving (Show)

data L2BF = L2ValInc   !Int64 !Word8
          | L2IOOutput !Int64
          | L2IORead   !Int64
          | L2While    !Int64 [L2BF]
          | L2LowLevel [L3BF]
    deriving (Show)

data L3BF = L3ValInc !Word8
          | L3IOOutput
          | L3IORead
          | L3While [L3BF]
          | L3Move  !L3Ptr
          | L3PtrInc !Int64
    deriving (Show)

data L3Ptr = L3Abs !Int64
           | L3End
    deriving (Show)


l3_to_bf :: [L3BF] -> BF.Program
l3_to_bf prog = (flip evalState 0 . go) prog where
    go prog = concat <$> mapM f prog
    f (L3ValInc w) = pure [BF.ValInc w]
    f (L3IOOutput) = pure [BF.IOOutput]
    f (L3IORead  ) = pure [BF.IORead]
    f (L3While cs) =  do { x <- go cs; pure [BF.While x]; }
    f (L3PtrInc i) = pure [BF.PtrInc i]
    f (L3Move ptr) = case ptr of L3Abs x -> move x
                                 L3End   -> move maxPtr
    move :: Int64 -> State Int64 [BF.BF]
    move n = do curr <- get
                put n
                return [BF.PtrInc (n - curr)]

    maxPtr = maxList prog
    maxList = foldl' maxTwo 0
    maxTwo !acc (L3ValInc _) = acc
    maxTwo !acc (L3IOOutput) = acc
    maxTwo !acc (L3IORead  ) = acc
    maxTwo !acc (L3While xs) = max acc (maxList xs)
    maxTwo !acc (L3Move (L3Abs i)) = max acc i
    maxTwo !acc (L3Move (L3End))   = acc
    maxTwo !acc (L3PtrInc _) = acc


l2_to_l3 :: [L2BF] -> [L3BF]
l2_to_l3 xs = convert xs where
        convert = concatMap go

        go (L2ValInc   i w) = [L3Move (L3Abs i), L3ValInc w]
        go (L2IOOutput i  ) = [L3Move (L3Abs i), L3IOOutput]
        go (L2IORead   i  ) = [L3Move (L3Abs i), L3IORead  ]
        go (L2While   i xs) = [L3Move (L3Abs i), L3While (convert xs ++ [L3Move (L3Abs i)])]
        go (L2LowLevel  xs) = xs

l1optimizer_simple :: [L1BF] -> [L1BF]
l1optimizer_simple = id

l1_to_l2 :: [L1BF] -> [L2BF]
l1_to_l2 = concatMap comp where
        local0 = 0
        transAddr f x@L3ValInc{}   = x
        transAddr f x@L3IOOutput{} = x
        transAddr f x@L3IORead{}   = x
        transAddr f x@L3PtrInc{}   = x
        transAddr f (L3While cs)   = L3While (map (transAddr f) cs)
        transAddr f x@(L3Move L3End) = x
        transAddr f (L3Move (L3Abs i)) = L3Move (L3Abs (f i))
        comp (L1LowLevel xs) = [L2LowLevel (map (transAddr (+16)) xs)]
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

data FunctionDef = FunctionDef [Param] [Statement]

data CompilerError = VariableNotFound {- `stack` trace -} [Ident] !Ident
                   | FuckedUp
            deriving (Show)

compile :: CompilerSettings -> Program -> Either CompilerError BF.Program
compile s p = fmap (\(_, _, _, p) -> p) (compile_complete s p)

moduleLookup :: [Module] -> ModuleName -> Module
moduleLookup xs m = case find (\(Module name _) -> name == m) xs of
                        Nothing -> error $ "Module " ++ m ++ " not found"
                        Just v  -> v

compile_complete :: CompilerSettings -> Program -> Either CompilerError ([L1BF], [L2BF], [L3BF], BF.Program)
compile_complete  _ (Program mods) = Right (flip evalState (CK [] M.empty []) $ do
                                genCompilerStack mods (moduleLookup mods "Main")
                                mainRes <- dk_declare "#main_res"
                                main    <- dk_lookup_fun "Main.main"
                                l1 <- compileFuncall mainRes main []
                                let l2 = l1_to_l2 l1
                                let l3 = l2_to_l3 l2
                                let bf = l3_to_bf l3
                                return (l1,l2,l3,bf))

{- variables starting with # are reversed for compiler internals -}

addModuleName :: ModuleName -> String -> String
addModuleName new str | Nothing <- find (=='.') str = new ++ "." ++ str
                      | otherwise = str

addModuleName'p :: ModuleName -> [Param] -> [Param]
addModuleName'p name ps = map go ps
    where go (ByRef (Ident r)) = ByRef (Ident (addModuleName name r))
          go (ByValue (Ident r)) = ByValue (Ident (addModuleName name r))

addModuleName' :: ModuleName -> [Statement] -> [Statement]
addModuleName' name = map goS where
    goS (CallE e) = CallE (goE e)
    goS (EBF bf) = EBF (goBF bf)
    goS (Decl id) = Decl (add id)
    goS (Assign id ex) = Assign (add id) (goE ex)
    goS (While ex so) = While (goE ex) (map goS so)
    goS (IfThenElse cond true false) = IfThenElse (goE cond) (map goS true) (map goS false)
    goS (Return re) = Return (goE re)

    goE (VL id) = VL (add id)
    goE (Lit o) = Lit o
    goE (CallF f ex) = CallF (add f) (map goE ex)

    goBF (EBValInc ptr w) = EBValInc (add'ptr ptr) w
    goBF (EBIOOutput ptr) = EBIOOutput (add'ptr ptr)
    goBF (EBIORead ptr) = EBIORead (add'ptr ptr)
    goBF (EBWhile ptr ws) = EBWhile (add'ptr ptr) (map goBF ws)
    goBF (EBSet ptr w) = EBSet (add'ptr ptr) w
    goBF (EBCopy p1 p2) = EBCopy (add'ptr p1) (add'ptr p2)
    goBF (EBMove p1 p2) = EBCopy (add'ptr p1) (add'ptr p2)
    goBF (EBLowLevel low) = EBLowLevel (map goLow low)

    goLow (ELLValInc w) = ELLValInc w
    goLow (ELLIOOutput) = ELLIOOutput
    goLow (ELLIORead)   = ELLIORead
    goLow (ELLWhile w)  = ELLWhile (map goLow w)
    goLow (ELLPtrInc i)  = ELLPtrInc i
    goLow (ELLMove ptrx) = ELLMove (add'x ptrx)

    add (Ident i) = Ident (addModuleName name i)
    
    add'ptr (EAbs i) = EAbs i
    add'ptr (EIndir i) = EIndir (add i)

    add'x (EAbsX i) = EAbsX i
    add'x (EIndirX i) = EIndirX (add i)
    add'x (EEndX) = EEndX
 
genCompilerStack :: [Module] -> Module -> State CompilerStack ()
genCompilerStack mods (Module name defs) =
                                   do CK stk funs lods <- get
                                      case find (\n -> n == name) lods of
                                        Just reg -> return () -- module already registered
                                        Nothing -> do
                                            put (CK stk funs (name : lods))
                                            dk_enter ("module_" ++ name)
                                            forM_ defs $ \x -> case x of
                                                Import nm -> genCompilerStack mods (moduleLookup mods nm)
                                                Declaration (Ident n) -> dk_declare (addModuleName name n) >> return ()
                                                Function (Ident xid) params stms -> dk_declare_fun (addModuleName name xid) (addModuleName'p name params) (addModuleName' name stms)

data CompilerStack = CK {
     ck_curr_stack :: [(String,M.Map String Int64)] {- every element represents a block + the name of it -}
    ,ck_funs       :: M.Map String FunctionDef
    ,ck_regs_mods  :: [ModuleName]
}


{- main operations done on the CS
    * enter a new scope
    * leave a scope [and thus "free" the stack variables]
    * lookup a variable
    * declare a variable
-}
dk_enter :: String -> State CompilerStack ()
dk_enter snam = modify (\(CK xs funs rgs) -> CK ((snam,M.empty) : xs) funs rgs)

{- NOTE: if we're at the "top-level" scope, we simply do nothing -}
dk_leave :: State CompilerStack ()
dk_leave = modify (\(CK xs funs rgs) -> CK (case xs of [x] -> [x] ; (_ : xs) -> xs) funs rgs)

dk_lookup :: String -> State CompilerStack Int64
dk_lookup xid = do CK xs _ _ <- get
                   let go [] = error $ "Variable not found, should never happen \nCalltrace: " ++ show (map fst xs)
                       go ((_,m):ms) = case M.lookup xid m of
                                        Nothing -> go ms
                                        Just p  -> p
                   return (go xs)

dk_declare_fun :: String -> [Param] -> [Statement] -> State CompilerStack ()
dk_declare_fun str params stms = modify (\(CK xs funs rgs) ->
                                                CK xs (M.insert str (FunctionDef params stms) funs) rgs)


dk_alias :: String -> Int64 -> State CompilerStack Int64
dk_alias xid addr = modify (\(CK xs funs rgs) ->
                            CK (f xs) funs rgs) >> dk_lookup xid
    where f q@((s, m) : ms) = (s , M.insert xid addr m) : ms
          
dk_declare :: String -> State CompilerStack Int64
dk_declare xid = modify (\(CK xs funs rgs) ->
                            CK (f xs) funs rgs) >> dk_lookup xid
    where f q@((s, m) : ms) = (s , M.insert xid (getCurrMax q + 1) m) : ms
          getCurrMax :: [(String, M.Map String Int64)] -> Int64
          getCurrMax [] = 16 {- magic number, should be obtained from CompilerSettings -}
          getCurrMax ((_, m):xs) = case M.elems m of
                                        [] -> getCurrMax xs
                                        mm -> max (maximum mm) (getCurrMax xs) {- because aliases -}


dk_lookup_fun :: String -> State CompilerStack FunctionDef
dk_lookup_fun  xid =
                    do CK _ funs _ <- get
                       case M.lookup xid funs of
                            Nothing -> error $ "Function should be around here (" ++ xid ++ ")"
                            Just  v -> return v


compileExpr :: Int64 -> Expr -> State CompilerStack [L1BF]
compileExpr resAddr (VL (Ident name)) = do i <- dk_lookup name
                                           return [L1Copy resAddr i]
compileExpr resAddr (Lit w)           = do return [L1Set  resAddr w]
compileExpr resAddr (CallF (Ident n) exps) =  do dk_enter ("#funcall_expr_" ++ n)
                                                 fun@(FunctionDef params _) <- dk_lookup_fun n
                                                 xs <- forM (zip [1..] (zip params exps)) $ \(n,(p, ex)) ->
                                                                    case (p, ex) of
                                                                         (ByRef{}, VL (Ident i)) -> (\i -> ([], i)) <$> dk_lookup i
                                                                         (ByValue{},_) -> do
                                                                                i_n <- dk_declare ("#i_" ++ show n)
                                                                                xcd <- compileExpr i_n ex
                                                                                return (L1Set i_n 0 : xcd, i_n)
                                                                         (_, _) -> error "Passing by reference some weird shit, FIX this later"
                                                 res <- dk_declare "#res"
                                                 code <- compileFuncall res fun (map snd xs)
                                                 dk_leave
                                                 return (
                                                    [L1Set res 0
                                                    ] ++ concatMap fst xs ++ code ++
                                                    [L1Copy resAddr res
                                                    ])

compileFuncall :: Int64 -> FunctionDef -> [Int64] -> State CompilerStack [L1BF]
compileFuncall resAddr (FunctionDef params stms) paramsAddrs = do
    dk_enter "#funcall"
    initCode <- fmap concat $ forM (zip params paramsAddrs) $ \(p,dr) -> do
                                    case p of
                                         ByValue (Ident r) -> do
                                             new_addr <- dk_declare r
                                             return [L1Copy new_addr dr]
                                         ByRef   (Ident r) -> do
                                             dk_alias r dr
                                             return []
    mainCode <- compileStatements resAddr stms
    dk_leave
    return (initCode ++ mainCode)

compileEBF :: EBF -> State CompilerStack [L1BF]
compileEBF x = (\w -> [w]) <$> go x where

    getPtr :: EPtr -> State CompilerStack Int64
    getPtr (EAbs i) = return i
    getPtr (EIndir (Ident x)) = dk_lookup x

    getPtrX :: EPtrX -> State CompilerStack L3Ptr
    getPtrX (EAbsX i)  = return $ L3Abs i
    getPtrX (EIndirX (Ident x)) = L3Abs <$> dk_lookup x
    getPtrX (EEndX)             = return L3End

    go :: EBF -> State CompilerStack L1BF
    go (EBValInc ptr w) = (\i -> L1ValInc i w) <$> getPtr ptr
    go (EBIOOutput ptr) = L1IOOutput <$> getPtr ptr
    go (EBIORead   ptr) = L1IORead   <$> getPtr ptr
    go (EBWhile  ptr xs) = (\i rs -> L1While i rs) <$> getPtr ptr <*> (mapM go xs)
    go (EBSet    ptr w)  = (\i -> L1Set i w) <$> getPtr ptr
    go (EBCopy   p1 p2)  = (\i1 i2 -> L1Copy i1 i2) <$> getPtr p1 <*> getPtr p2
    go (EBMove   p1 p2)  = (\i1 i2 -> L1Move i1 i2) <$> getPtr p1 <*> getPtr p2
    go (EBLowLevel xs)   = L1LowLevel <$> compileLL xs

    compileLL :: [ELLBF] -> State CompilerStack [L3BF]
    compileLL prog = mapM ll prog

    ll :: ELLBF -> State CompilerStack L3BF
    ll (ELLValInc w)  = return $ L3ValInc w
    ll (ELLIOOutput)  = return $ L3IOOutput
    ll (ELLIORead)    = return $ L3IORead
    ll (ELLPtrInc ix) = return $ L3PtrInc ix
    ll (ELLWhile code) = (\code -> L3While code) <$> compileLL code
    ll (ELLMove ptrx)  = L3Move <$> getPtrX ptrx

compileStatements :: Int64 -> [Statement] -> State CompilerStack [L1BF]
compileStatements resAddr [] = return []
compileStatements resAddr (x:xs) =
    case x of
         Return expr -> compileExpr resAddr expr
         EBF ebf -> (++) <$> compileEBF ebf <*> compileStatements resAddr xs
         CallE  expr -> (++) <$> compileExpr resAddr expr <*> compileStatements resAddr xs
         Decl   (Ident n) -> do dk_declare n
                                compileStatements resAddr xs
         Assign (Ident n) expr -> do ir <- dk_lookup n
                                     (++) <$> compileExpr ir expr <*> compileStatements resAddr xs
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
                                                ,L1Set    condE 0
                                                ])
                                            ,L1While temp0
                                                (falseCode ++
                                                [L1ValInc temp0 (-1)
                                                ])
                                            ])

