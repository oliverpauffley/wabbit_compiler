{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Blocks where

import           Control.Monad.State.Lazy
import           Data.Text                (Text)
import qualified Expr                     as E
import           Prelude                  hiding (EQ, LT)
import           Prettyprinter
import           PrintExpr                (showStmt)

data INSTRUCTION
  = PUSH Int
  | ADD
  | MUL
  | LT
  | EQ
  | LOADGLOBAL Text
  | LOADLOCAL Text
  | STOREGLOBAL Text
  | STORELOCAL Text
  | CALL Text Int
  | PRINT
  | RETURN
  | LOCAL Text
  | GOTO BlockLabel
  | CBRANCH BlockLabel BlockLabel
  deriving (Eq, Show)

data Block = Block BlockLabel [INSTRUCTION]
  deriving (Eq, Show)

data StackCode = StackCode
  { _topLevelGlobals :: [E.Stmt],
    _stackCode       :: [Structure]
  }
  deriving (Eq, Show)

showStack :: StackCode -> Doc ann
showStack (StackCode globals code) = vsep (map showStmt globals) <> line <> vsep (map showStructure code)

data Structure
  = Func Text [Text] Structure
  | Structs [Structure]
  | If {_case :: Block, _then :: Structure, _else :: Maybe Structure}
  | While {_case :: Block, _block :: Structure}
  | BL Block
  deriving (Eq, Show)

showStructure :: Structure -> Doc ann
showStructure (Func n vars body) =
  "func"
    <+> pretty n
    <> parens (hsep (punctuate comma (map pretty vars)))
    <+> "{"
    <> nest 4 (line <> showStructure body)
    <> line
    <> "}"
showStructure (Structs ss) = vsep (punctuate line (map showStructure ss))
showStructure (If c t e) =
  "If"
    <+> showBlock c
    <+> "{"
    <+> vsep
      [nest 4 (line <> showStructure t), "}", elseIf e]
  where
    elseIf (Just elseexpr) = space <> "else" <+> "{" <> line <> indent 4 (showStructure elseexpr)
    elseIf Nothing = mempty
showStructure (While c t) = "while" <+> showBlock c <+> "{" <+> vsep [nest 4 (line <> showStructure t), "}"]
showStructure (BL block) = showBlock block

showBlock :: Block -> Doc ann
showBlock (Block l ins) = pretty (show l) <> indent 2 (brackets (hsep (punctuate comma (map (pretty . show) ins))))

fromExpr :: E.Expr -> INSTRUCTION
fromExpr (E.PUSH i)        = PUSH i
fromExpr E.ADD             = ADD
fromExpr E.MUL             = MUL
fromExpr E.LT              = LT
fromExpr E.EQ              = EQ
fromExpr (E.LOADGLOBAL n)  = LOADGLOBAL n
fromExpr (E.LOADLOCAL n)   = LOADLOCAL n
fromExpr (E.STORELOCAL n)  = STORELOCAL n
fromExpr (E.STOREGLOBAL n) = STOREGLOBAL n
fromExpr (E.CALL n i)      = CALL n i
fromExpr E.PRINT           = PRINT
fromExpr E.RETURN          = RETURN
fromExpr (E.LOCAL n)       = LOCAL n
fromExpr e                 = error ("called with no INSTRUCTION " <> show e)

newtype BlockLabel = BlockLabel Int
  deriving (Eq)

instance Show BlockLabel where
  show (BlockLabel i) = "L" <> show i

instance Pretty BlockLabel where
  pretty b = pretty $ show b

instance Enum BlockLabel where
  toEnum :: Int -> BlockLabel
  toEnum = BlockLabel
  fromEnum :: BlockLabel -> Int
  fromEnum (BlockLabel i) = i

-- | labels blocks with the correct label
newtype Blocker a = Blocker
  {_runBlocker :: State BlockLabel a}
  deriving (Functor, Applicative, Monad, MonadState BlockLabel)

incBL :: Blocker ()
incBL = do
  c <- get
  put (succ c)
  return ()

-- gets the next label and increments
getInc :: Blocker BlockLabel
getInc = do
  next <- get
  incBL
  return next

-- | turn the ast into a more stack machine code block. First dealing with the top level
-- block then into the entire tree.
statementsToStackCode :: E.Stmt -> Blocker StackCode
statementsToStackCode (E.BlockStmt ss) =
  let (vars, funcs) = topLevel ([], []) ss
   in do
        StackCode vars <$> labelFuncs [] funcs

-- pull out any toplevel global statements and then the top level functions
topLevel :: ([E.Stmt], [E.Stmt]) -> [E.Stmt] -> ([E.Stmt], [E.Stmt])
topLevel (vars, funcs) [] = (vars, funcs)
topLevel (vars, funcs) (v@(E.GlobalVar _) : ss) = topLevel (vars ++ [v], funcs) ss
topLevel (vars, funcs) (f@(E.FuncStmt {}) : ss) = topLevel (vars, funcs ++ [f]) ss
topLevel _ a = error ("calling on the wrong stuff! " <> show a)

labelFuncs :: [Structure] -> [E.Stmt] -> Blocker [Structure]
labelFuncs ss [] = return ss
labelFuncs ss (E.FuncStmt n vs e : fs) = do
  struct <- labelBlockStmt e
  labelFuncs (ss ++ [Func n vs struct]) fs
labelFuncs _ s = error ("calling on the wrong stuff! " <> show s)

labelBlockStmt :: E.Stmt -> Blocker Structure
labelBlockStmt (E.BlockStmt ss) = labelWithinBlockStmts ss >>= (return . Structs)

labelWithinBlockStmts :: [E.Stmt] -> Blocker [Structure]
labelWithinBlockStmts = traverse labelStmts

labelStmts :: E.Stmt -> Blocker Structure
labelStmts (E.ExprStmt a) = BL <$> labelBlock a
labelStmts (E.IfStmt c t e) = If <$> labelBlock c <*> labelBlockStmt t <*> traverse labelBlockStmt e
labelStmts (E.WhileStmt c t) = While <$> labelBlock c <*> labelBlockStmt t

labelBlock :: E.Expr -> Blocker Block
labelBlock (E.INSTRUCTIONS xs) = do
  let ins = map fromExpr xs
  label <- getInc
  return $ Block label ins

data StackCodeError = Nil
  deriving (Show)

blocker :: E.Stmt -> Either StackCodeError StackCode
blocker stmt = Right $ evalState (_runBlocker $ statementsToStackCode stmt) (BlockLabel 0)

controlFlower :: StackCode -> Either StackCodeError StackCode
controlFlower = Right . controlFlow

controlFlow :: StackCode -> StackCode
controlFlow (StackCode vars ss) = StackCode vars (addControlFlowFuncs ss)

addControlFlowFuncs :: [Structure] -> [Structure]
addControlFlowFuncs [] = []
addControlFlowFuncs ((Func n vs (Structs s)) : ss) = Func n vs (addControlStructure (Structs []) Nothing (reverse s)) : addControlFlowFuncs ss

-- Update the structures within a function call.
addControlStructure :: Structure -> Maybe BlockLabel -> [Structure] -> Structure
addControlStructure acc _ [] = acc
-- Handle the end! get the block label for the final statement and then we can use this elsewhere
addControlStructure (Structs acc) Nothing (b : rest) = addControlStructure (Structs (b : acc)) (Just (getFirstBlockLabel b)) rest
-- for a while statement we need to
--    - get the label for the first part of the structure within the while
--    - update the test block to point to the pLabel we have been given
--    - update the inside block to point outwards to the test block (this is done with recursion back into this func)
addControlStructure (Structs acc) (Just pLabel) ((While (Block l bs) inside@(Structs is)) : rest) =
  let iLabel = getFirstBlockLabel inside
   in addControlStructure (Structs (BL (Block l (updateTest (iLabel, pLabel) bs)) : addControlStructure (Structs []) (Just l) (reverse is) : acc)) (Just l) rest
-- for an if statement we need to
--    - update both (if they exist) cases of the statement with goto's to our pLabel
--    - update the test to point to the two cases (or the end if no else)
addControlStructure (Structs acc) (Just pLabel) ((If (Block l bs) then'@(Structs ts) (Just else'@(Structs es))) : rest) =
  let thenLabel = getFirstBlockLabel then'
      elseLabel = getFirstBlockLabel else'
      updatedTest = BL (Block l (updateTest (thenLabel, elseLabel) bs))
      updatedThen = addControlStructure (Structs []) (Just pLabel) (reverse ts)
      updatedElse = addControlStructure (Structs []) (Just pLabel) (reverse es)
   in addControlStructure
        (Structs (updatedTest : updatedThen : updatedElse : acc))
        (Just l)
        rest
-- If no else
addControlStructure (Structs acc) (Just pLabel) ((If (Block l bs) then'@(Structs ts) Nothing) : rest) =
  let thenLabel = getFirstBlockLabel then'
      updatedTest = BL (Block l (updateTest (thenLabel, pLabel) bs))
      updatedThen = addControlStructure (Structs []) (Just pLabel) (reverse ts)
   in addControlStructure
        (Structs (updatedTest : updatedThen : acc))
        (Just l)
        rest
-- for a list of structure we should
--     - add a goto to the pLabel of the last statement
--     - get the label of the first block to pass upwards
addControlStructure (Structs acc) (Just pLabel) (s@(Structs ss) : rest) =
  let nextLabel = getFirstBlockLabel s
      updatedInternals = updateFinalStmtWithGOTO pLabel (reverse ss)
   in addControlStructure (Structs (updatedInternals : acc)) (Just nextLabel) rest
-- for a block add a goto and get the label to pass up
addControlStructure (Structs acc) (Just pLabel) ((BL (Block l bs)) : rest) =
  let updatedInternals = BL (Block l (updateBlock pLabel bs))
   in addControlStructure (Structs (updatedInternals : acc)) (Just l) rest

-- | take a test and update it's instructions to a branch to go the given labels
-- first is on success, second on failure
updateTest :: (BlockLabel, BlockLabel) -> [INSTRUCTION] -> [INSTRUCTION]
updateTest (inside, beyond) bs = bs ++ [CBRANCH inside beyond]

-- | update block to add goto
updateBlock :: BlockLabel -> [INSTRUCTION] -> [INSTRUCTION]
updateBlock label bs = bs ++ [GOTO label]

-- | Extract the block label from the first part of a structure
getFirstBlockLabel :: Structure -> BlockLabel
getFirstBlockLabel (Structs (s : _))     = getFirstBlockLabel s
getFirstBlockLabel (If (Block l _) _ _)  = l
getFirstBlockLabel (While (Block l _) _) = l
getFirstBlockLabel (BL (Block l _))      = l

-- | Should be passed a reversed structures contents
updateFinalStmtWithGOTO :: BlockLabel -> [Structure] -> Structure
updateFinalStmtWithGOTO label (s : ss) = Structs (reverse (appendGOTO label s : ss))

-- | add a go to as the last instruction of a block.
appendGOTO :: BlockLabel -> Structure -> Structure
appendGOTO l (BL (Block bl bs)) = BL (Block bl (bs ++ [GOTO l]))
