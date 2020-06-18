{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Kaleidoscope.Codegen where

import Control.Applicative
import Control.Monad.State
import qualified Data.ByteString.Short as S
import Data.Function
import Data.List
import Data.ByteString.UTF8 as BU
import qualified Data.Map as Map
import Data.String
import Data.Word
import LLVM.AST
import qualified Kaleidoscope.AST as S
import qualified LLVM.AST as AST
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import LLVM.AST.Global
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Float as F
import Control.Monad.Trans.Except (ExceptT)
import qualified Data.ByteString as B
import LLVM (moduleAST, moduleTargetAssembly, moduleLLVMAssembly, withModuleFromAST)
import LLVM.Context (withContext)
import LLVM.Target (TargetMachine)
import LLVM.PassManager (runPassManager, withPassManager, defaultCuratedPassSetSpec, optLevel, PassSetSpec)
import LLVM.Analysis (verify)

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: S.ShortByteString -> AST.Module
emptyModule label = defaultModule {moduleName = label}

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s {moduleDefinitions = defs ++ [d]}

define :: Type -> S.ShortByteString -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body =
  addDefn
    $ GlobalDefinition
    $ functionDefaults
      { name = Name label,
        parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False),
        returnType = retty,
        basicBlocks = body
      }
external :: Type -> S.ShortByteString -> [(Type, Name)] -> LLVM ()
external retty label argtys =
  addDefn
    $ GlobalDefinition
    $ functionDefaults
      { name = Name label,
        linkage = L.External,
        parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False),
        returnType = retty,
        basicBlocks = []
      }

-- IEEE 754 double
double :: Type
double = FloatingPointType DoubleFP

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map S.ShortByteString Int

uniqueName :: S.ShortByteString -> Names -> (S.ShortByteString, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (  S.toShort $ BU.fromString (BU.toString (S.fromShort nm) ++ show ix), Map.insert nm (ix + 1) ns)

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(String, Operand)]

data CodegenState = CodegenState
  { currentBlock :: Name, -- Name of the active block to append to
    blocks :: Map.Map Name BlockState, -- Blocks for function
    symtab :: SymbolTable, -- Function scope symbol table
    blockCount :: Int, -- Count of basic blocks
    count :: Word, -- Count of unnamed instructions
    names :: Names -- Name Supply
  }
  deriving (Show)

data BlockState = BlockState
  { idx :: Int, -- Block index
    stack :: [Named Instruction], -- Stack of instructions
    term :: Maybe (Named Terminator) -- Block terminator
  }
  deriving (Show)

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen {runCodegen :: State CodegenState a}
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show l
    
entryBlockName :: S.ShortByteString
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s {count = 1 + i}
  return $ i + 1

instr :: Instruction -> Codegen (Operand)
instr ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk {stack = (ref := ins) : i})
  return $ local ref

ninstr :: Instruction -> Codegen ()
ninstr ins = do
  n <- fresh
  blk <- current
  let i = stack blk
  modifyBlock (blk {stack = Do ins : i})

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk {term = Just trm})
  return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock
addBlock :: S.ShortByteString -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s ->
    s
      { blocks = Map.insert (Name qname) new bls,
        blockCount = ix + 1,
        names = supply
      }
  return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s {currentBlock = bname}
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s {blocks = Map.insert active new (blocks s)}

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s {symtab = [(var, x)] ++ lcls}
 
getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

-------------------------------------------------------------------------------

-- References
local :: Name -> Operand
local = LocalReference double

global :: Name -> C.Constant
global = C.GlobalReference double

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference double

--externf :: Name -> Codegen Operand
--externf (UnName name) = do
--  getvar (show name)
--externf (Name name) = do
--  getvar (toString name)
-- Arithmetic and Constants
fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd noFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub noFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul noFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv noFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

cons :: C.Constant -> Operand
cons = ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen ()
store ptr val = ninstr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

--
toshort = S.toShort . BU.fromString

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name (toshort x)))

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) = do
  void $ define double (toshort name) (toSig args) bls
  where
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca double
        store var (LocalReference double (AST.Name (toshort a)))
        assign a var
      cgen body >>= ret
codegenTop (S.Extern name args) = do
  void $ external double (toshort name) (toSig args)
codegenTop exp = do
  void $ define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

binops =
  Map.fromList
    [ ("+", fadd),
      ("-", fsub),
      ("*", fmul),
      ("/", fdiv),
      ("<", lt)    ]

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.UnaryOp op a) = do
  cgen $ S.Call ("unary" ++ op) [a]
cgen (S.BinaryOp "=" (S.Var var) val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  return cval
cgen (S.BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
cgen (S.Var x) = getvar x >>= load
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name (toshort fn))) largs
cgen _ = undefined

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------
passes :: PassSetSpec
passes = defaultCuratedPassSetSpec {optLevel = Just 3}


codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  
  withModuleFromAST context newast $ \m -> do
    withPassManager passes $ \pm -> do
      verify m
      runPassManager pm m
      optmod <- moduleAST m
      s <- moduleLLVMAssembly m
      B.putStrLn s
      return optmod
 
    return newast
  where
    modn = mapM codegenTop fns
    newast = runLLVM mod modn