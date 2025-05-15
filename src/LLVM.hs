{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module LLVM where

import qualified Blocks                   as B
import           Control.Monad            (replicateM)
import           Control.Monad.State.Lazy
import           Data.Foldable            (foldl')
import           Data.Maybe               (catMaybes)
import           Data.Text                (Text)
import qualified Expr                     as E
import           Prettyprinter

newtype IResult = IResult Int

instance Enum IResult where
  toEnum :: Int -> IResult
  toEnum = IResult
  fromEnum :: IResult -> Int
  fromEnum (IResult i) = i

instance Pretty IResult where
  pretty (IResult i) = "%." <> pretty (show i)

data Input = InputR IResult | InputI Int | InputT Text

toIResult :: Input -> IResult
toIResult (InputR i) = i
toIResult (InputI _) = error "didn't get a register when we needed one"
toIResult (InputT _) = error "didn't get a register when we needed one"

instance Pretty Input where
  pretty (InputR i) = pretty i
  pretty (InputI i) = pretty i
  pretty (InputT i) = "%" <> pretty i

data BinOp = Add | Mul | LessThan | Equal

instance Pretty BinOp where
  pretty Add      = "add"
  pretty Mul      = "mul"
  pretty LessThan = "icmp slt"
  pretty Equal    = "icmp eq"

data Env = Local | Global

instance Pretty Env where
  pretty Local  = "%"
  pretty Global = "@"

newtype LLVMProgram = LLVMProgram [LLVMStructure]

instance Pretty LLVMProgram where
  pretty :: LLVMProgram -> Doc ann
  pretty (LLVMProgram structs) = vsep ("declare i32 @_print_int(i32)" <> line : map pretty structs)

data LLVMStructure = Func {_funcname :: Text, _funcargs :: [Text], _entry :: LLVMBlock, _blocks :: [LLVMBlock]} | GlobalVar Text

instance Pretty LLVMStructure where
  pretty :: LLVMStructure -> Doc ann
  pretty (Func name args entry bs) =
    "define"
      <+> "i32"
      <+> "@"
      <> pretty name
      <> parens (hsep (punctuate comma (prettyFuncArgs args)))
      <+> "{"
      <> vsep [line <> pretty entry, line <> vsep (map pretty bs), "}"]
  pretty (GlobalVar name) = "@" <> pretty name <+> "=" <+> "global" <+> "i32" <+> "0"

prettyFuncArgs :: (Pretty a) => [a] -> [Doc ann]
prettyFuncArgs = map (\a -> "i32" <+> "%" <> pretty a)

data LLVMLabel = BL B.BlockLabel | EntryLabel

instance Pretty LLVMLabel where
  pretty (BL lb)    = pretty lb
  pretty EntryLabel = "entry"

data LLVMBlock = LLVMBlock {_label :: LLVMLabel, _instructions :: [LLVMOperation]}

instance Pretty LLVMBlock where
  pretty (LLVMBlock lb bs) = pretty lb <> ":" <> nest 4 (line <> vsep (map pretty bs))

data LLVMOperation
  = BinOp {_op :: BinOp, _result :: IResult, _left :: Input, _right :: Input}
  | Allocate Text
  | Load {_result :: IResult, _name :: Text, _env :: Env}
  | Store {_name :: Text, _value :: Input, _env :: Env}
  | Goto {_target :: B.BlockLabel}
  | Branch {_test :: IResult, _true :: B.BlockLabel, _false :: B.BlockLabel}
  | Call {_result :: IResult, _funcName :: Text, _args :: [Input]}
  | Return {_value :: Input}
  | Print {_value :: Input}

resultEq :: IResult -> Doc ann
resultEq i = pretty i <+> "="

instance Pretty LLVMOperation where
  pretty (BinOp op res l r) = resultEq res <+> pretty op <+> "i32" <+> pretty l <> comma <+> pretty r
  pretty (Allocate name) = "%" <> pretty name <+> "=" <+> "alloca" <+> "i32"
  pretty (Load res name env) = resultEq res <+> "load" <+> "i32" <> comma <+> "i32*" <+> pretty env <> pretty name
  pretty (Store name val env) = "store" <+> "i32" <+> pretty val <> comma <+> "i32*" <+> pretty env <> pretty name
  pretty (Goto tar) = "br" <+> "label" <+> "%" <> pretty tar
  pretty (Branch test true false) = "br" <+> "i1" <+> pretty test <> comma <+> "label" <+> "%" <> pretty true <> comma <+> "label" <+> "%" <> pretty false
  pretty (Call res func args) =
    resultEq res
      <+> "call"
      <+> "i32"
      <+> parens (hcat (punctuate comma (replicate (length args) "i32")))
      <+> "@"
      <> pretty func
      <> parens (hcat (punctuate comma (prettyArgs args)))
  pretty (Return inp) = "ret" <+> "i32" <+> pretty inp
  pretty (Print val) = "call" <+> "i32" <+> "(i32)" <+> "@_print_int(i32" <+> pretty val <> ")"

prettyArgs :: (Pretty a) => [a] -> [Doc ann]
prettyArgs = map (\a -> "i32" <+> pretty a)

data LLVMState = LLVMState {_registerValue :: IResult, _stack :: [Input]}

newLLVMState :: LLVMState
newLLVMState = LLVMState (IResult 0) []

newtype LLVMConverter a = LLVMConverter
  { _runLLVMConverter :: State LLVMState a
  }
  deriving (Functor, Applicative, Monad, MonadState LLVMState)

-- | gets a new register to store values and increments the counter for the next call
getRegister :: LLVMConverter IResult
getRegister = do
  LLVMState {..} <- get
  put $ LLVMState (succ _registerValue) _stack
  return _registerValue

push :: Input -> LLVMConverter ()
push i = do
  LLVMState {..} <- get
  put $ LLVMState _registerValue (i : _stack)

-- | unsafe :shrug:
pop :: LLVMConverter Input
pop = do
  state $ \(LLVMState res (top : rest)) -> (top, LLVMState res rest)

data LLVMError = Nil
  deriving (Show)

convertToLLVM :: B.StackCode -> Either LLVMError LLVMProgram
convertToLLVM code = Right $ evalState (_runLLVMConverter $ convertToLLVM' code) newLLVMState

convertToLLVM' :: B.StackCode -> LLVMConverter LLVMProgram
convertToLLVM' (B.StackCode globals funcs) = do
  funcs' <- mapM unpackFuncs funcs
  return $ LLVMProgram (convertGlobals globals ++ funcs')

-- TODO actually convert
convertGlobals :: [E.Stmt] -> [LLVMStructure]
convertGlobals = foldl' (\acc (E.GlobalVar n) -> acc ++ [GlobalVar n]) []

-- | from our stack conversion work we only have functions containing blocks
-- so we should unpack them all into a better structure.
-- TODO convert here too?
unpackFuncs :: B.Structure -> LLVMConverter LLVMStructure
unpackFuncs (B.Func name args b@(B.Structs bs)) =
  let blocks = map unpackBlocks bs
      renamedArgs = mapFunctionArgs args
      pairedLabels = zip args renamedArgs
      firstLabel = B.getFirstBlockLabel b
      entry = buildEntryBlock pairedLabels firstLabel
   in do
        blocks' <- mapM convertBlock (concat blocks)
        return $ Func name renamedArgs entry blocks'

-- | rename function argument from `x` to `.arg_x`
mapFunctionArgs :: [Text] -> [Text]
mapFunctionArgs = map (".arg_" <>)

buildEntryBlock :: [(Text, Text)] -> B.BlockLabel -> LLVMBlock
buildEntryBlock args label =
  let instructions = concatMap createEntryAllocs args
   in LLVMBlock EntryLabel (instructions ++ [Goto label])

createEntryAllocs :: (Text, Text) -> [LLVMOperation]
createEntryAllocs (origLabel, newLabel) =
  [ Allocate origLabel,
    Store origLabel (InputT newLabel) Local
  ]

unpackBlocks :: B.Structure -> [B.Block]
unpackBlocks (B.BL b)             = [b]
unpackBlocks (B.Structs [B.BL b]) = [b]
unpackBlocks (B.Structs bs)       = concatMap unpackBlocks bs
unpackBlocks i                    = error $ "called with " ++ show i

convertBlock :: B.Block -> LLVMConverter LLVMBlock
convertBlock (B.Block label ins) = do
  ins' <- mapM convertInstructions ins
  return $ LLVMBlock (BL label) (catMaybes ins')

convertInstructions :: B.INSTRUCTION -> LLVMConverter (Maybe LLVMOperation)
convertInstructions (B.PUSH i) = push (InputI i) >> return Nothing
convertInstructions B.ADD = convertBin B.ADD
convertInstructions B.MUL = convertBin B.MUL
convertInstructions B.LT = convertBin B.LT
convertInstructions B.EQ = convertBin B.EQ
convertInstructions (B.LOADGLOBAL n) = convertLoad n Global
convertInstructions (B.LOADLOCAL n) = convertLoad n Local
convertInstructions (B.STOREGLOBAL n) = convertStore n Global
convertInstructions (B.STORELOCAL n) = convertStore n Local
convertInstructions (B.CALL name numArgs) = do
  args <- replicateM numArgs pop
  register <- getRegister
  push (InputR register)
  return $ Just (Call register name args)
convertInstructions B.PRINT = do
  val <- pop
  return $ Just (Print val)
convertInstructions B.RETURN = do
  val <- pop
  return $ Just (Return val)
convertInstructions (B.LOCAL n) = return $ Just (Allocate n)
convertInstructions (B.GOTO bl) = return $ Just (Goto bl)
convertInstructions (B.CBRANCH blT blF) = do
  r <- toIResult <$> pop
  return $ Just (Branch r blT blF)

convertStore :: Text -> Env -> LLVMConverter (Maybe LLVMOperation)
convertStore n env = do
  val <- pop
  return $ Just (Store n val env)

convertLoad :: Text -> Env -> LLVMConverter (Maybe LLVMOperation)
convertLoad n env = do
  register <- getRegister
  push (InputR register)
  return $ Just (Load register n env)

convertBin :: B.INSTRUCTION -> LLVMConverter (Maybe LLVMOperation)
convertBin op = do
  right <- pop
  left <- pop
  register <- getRegister
  push (InputR register)
  return $ Just (BinOp (convertBinOp op) register left right)

convertBinOp :: B.INSTRUCTION -> BinOp
convertBinOp B.ADD = Add
convertBinOp B.MUL = Mul
convertBinOp B.LT  = LessThan
convertBinOp B.EQ  = Equal
