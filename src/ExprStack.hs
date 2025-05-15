-- | ExprStack converts the AST expr into a more virtual machine format
module ExprStack where

import           Data.Foldable (foldl')
import           Data.List     (groupBy)
import           Expr
import           Prelude       hiding (EQ, LT)
import           Simplifier    (deblock)

exprIntr :: Expr -> Expr
exprIntr = go
  where
    go :: Expr -> Expr
    go (IntL i) = INSTRUCTIONS [PUSH i]
    go (IdentExpr (GlobalName n)) = INSTRUCTIONS [LOADGLOBAL n]
    go (IdentExpr (LocalName n)) = INSTRUCTIONS [LOADLOCAL n]
    go (IdentExpr n) = IdentExpr n
    go (FunCall n xs) = INSTRUCTIONS (unpackInstructions(map go xs) ++ [CALL n (length xs)])
    go (AssignExpr (GlobalName i) xs) = INSTRUCTIONS [go xs, STOREGLOBAL i]
    go (AssignExpr (LocalName i) xs) = INSTRUCTIONS [go xs, STORELOCAL i]
    go (BinExpr op l r) = case (op, go l, go r) of
      (Add, l', r') -> INSTRUCTIONS (combineInstrs (go l') (go r') ++ [ADD])
      (Mul, l', r') -> INSTRUCTIONS (combineInstrs (go l') (go r') ++ [MUL])
      (LessThan, l', r') -> INSTRUCTIONS (combineInstrs (go l') (go r') ++ [LT])
      (Eq, l', r') -> INSTRUCTIONS (combineInstrs (go l') (go r') ++ [EQ])
    go (Grouping expr) = go expr
    go a = a

unpackInstructions :: [Expr] -> [Expr]
unpackInstructions []                       = []
unpackInstructions ((INSTRUCTIONS xs): xss) = xs ++ unpackInstructions xss
unpackInstructions (xs: xss)                = xs : unpackInstructions xss

combineInstrs :: Expr -> Expr -> [Expr]
combineInstrs (INSTRUCTIONS xs) (INSTRUCTIONS ys) = xs ++ ys
combineInstrs x y                                 = [x, y]

changeExpr :: Stmt -> Stmt
changeExpr (ExprStmt e) = ExprStmt $ exprIntr e
changeExpr (PrintStmt e) = ExprStmt $ INSTRUCTIONS [exprIntr e, PRINT]
changeExpr (IfStmt c i e) = IfStmt (exprIntr c) (changeExpr i) (changeExpr <$> e)
changeExpr (WhileStmt c s) = WhileStmt (exprIntr c) (changeExpr s)
changeExpr (FuncStmt n vs s) = FuncStmt n vs (changeExpr s)
changeExpr (ReturnStmt e) = ExprStmt $ INSTRUCTIONS [exprIntr e, RETURN]
changeExpr (LocalVar n) = ExprStmt $ LOCAL n
-- we want to swap these around to push the value first
changeExpr (BlockStmt (var@(GlobalVar n) : expr@(ExprStmt (AssignExpr (GlobalName n') _)) : xs))
  | n == n' =
      changeExpr $ BlockStmt (expr : var : xs)
changeExpr (BlockStmt xs) = BlockStmt (map changeExpr xs)
changeExpr s = s

-- remove any nested INSTRUCTIONS
deNest :: Stmt -> Stmt
deNest (ExprStmt e)      = ExprStmt $ deNest' e
deNest (PrintStmt e)     = ExprStmt $ deNest' e
deNest (IfStmt c i e)    = IfStmt (deNest' c) (deNest i) (deNest <$> e)
deNest (WhileStmt c s)   = WhileStmt (deNest' c) (deNest s)
deNest (FuncStmt n vs s) = FuncStmt n vs (deNest s)
deNest (ReturnStmt e)    = ExprStmt $ deNest' e
deNest (LocalVar n)      = ExprStmt $ LOCAL n
deNest (BlockStmt xs)    = BlockStmt (map deNest xs)
deNest s                 = s

deNest' :: Expr -> Expr
deNest' (INSTRUCTIONS xs) = INSTRUCTIONS (deNestInstructions xs)
deNest' e                 = e

deNestInstructions :: [Expr] -> [Expr]
deNestInstructions []                        = []
deNestInstructions ((INSTRUCTIONS xs) : xss) = xs ++ deNestInstructions xss
deNestInstructions (xs : xss)                = xs : deNestInstructions xss

combineBlocksTopLevel :: Stmt -> Stmt
combineBlocksTopLevel (BlockStmt ss) = BlockStmt (go [] ss)
  where
    go :: [Stmt] -> [Stmt] -> [Stmt]
    go acc [] = acc
    go acc (g@(GlobalVar _) : ss') = go (acc ++ [g]) ss'
    go acc ((FuncStmt n vs s) : ss') = go (acc ++ [FuncStmt n vs (combineBlocks s)]) ss'
    go _ _ = error "called with the wrong stuff"
combineBlocksTopLevel _ = error "called with the wrong stuff"

-- | where blocks are filled with lists of INSTRUCTIONS combine them into one
combineBlocks :: Stmt -> Stmt
combineBlocks (FuncStmt n vs s) = FuncStmt n vs (combineBlocks s)
combineBlocks (IfStmt c t e) = IfStmt c (combineBlocks t) (combineBlocks <$> e)
combineBlocks (WhileStmt c t) = WhileStmt c (combineBlocks t)
combineBlocks (BlockStmt ss) = BlockStmt (combineBlocks' ss)
combineBlocks a = error ("called with the wrong stuff! " <> show a)

-- | helper for grouping expressions
isExprStmts :: Stmt -> Stmt -> Bool
isExprStmts (ExprStmt _) (ExprStmt _) = True
isExprStmts _s _a                     = False

-- | helper for identifing expressions
isExprStmt :: Stmt -> Bool
isExprStmt (ExprStmt _) = True
isExprStmt _s           = False

combineBlocks' :: [Stmt] -> [Stmt]
combineBlocks' = flattenExprs . blocks

blocks :: [Stmt] -> [[Stmt]]
blocks = groupBy isExprStmts

flattenExprs :: [[Stmt]] -> [Stmt]
flattenExprs [] = []
flattenExprs (ss : sss) | all isExprStmt ss = ExprStmt (combineBlockExprStmt ss) : flattenExprs sss
flattenExprs (ss : sss) = map combineBlocks ss ++ flattenExprs sss

-- | turns a block statement of multiple expression statments of instructions
-- into a single one.
combineBlockExprStmt :: [Stmt] -> Expr
combineBlockExprStmt ss =
  foldl'
    combineInstructions
    (INSTRUCTIONS [])
    ss

combineInstructions :: Expr -> Stmt -> Expr
combineInstructions (INSTRUCTIONS acc) ((ExprStmt (INSTRUCTIONS a))) = INSTRUCTIONS (acc ++ a)
combineInstructions (INSTRUCTIONS acc) ((ExprStmt a)) = INSTRUCTIONS (acc ++ [a])
combineInstructions _ a = error ("wat! " <> show a)

data ExprStackError = Nil
  deriving (Show)

exprStacker :: Stmt -> Either ExprStackError Stmt
exprStacker = Right <$> combineBlocksTopLevel . deblock . deNest . changeExpr
