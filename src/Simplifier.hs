module Simplifier where

import           Expr

-- | Fold constants looks for any addition or multiplication binary operations
-- where both subexprs are constants so that the entire expr can be simplified.
foldConstants :: Stmt -> Stmt
foldConstants (ExprStmt e) = ExprStmt $ foldExprConstants e
foldConstants (VarStmt i e) = VarStmt i (foldExprConstants <$> e)
foldConstants (BlockStmt ss) = BlockStmt $ map foldConstants ss
foldConstants (IfStmt c i e) = IfStmt (foldExprConstants c) (foldConstants i) (foldConstants <$> e)
foldConstants (WhileStmt e s) = WhileStmt (foldExprConstants e) (foldConstants s)
foldConstants (FuncStmt i vs s) = FuncStmt i vs (foldConstants s)
foldConstants (ReturnStmt e) = ReturnStmt (foldExprConstants e)
foldConstants (PrintStmt e) = PrintStmt (foldExprConstants e)
foldConstants s = s

foldExprConstants :: Expr -> Expr
-- foldExprConstants (IntL l) = IntL l
-- foldExprConstants (Grouping e) = Grouping (foldExprConstants e)
-- foldExprConstants (AssignExpr i e) = AssignExpr i (foldExprConstants e)
-- foldExprConstants (FunCall i e) = FunCall i (map foldExprConstants e)
-- -- foldExprConstants (BinExpr b l r) = case (b, foldExprConstants l, foldExprConstants r) of
-- --   -- (Add, IntL l', IntL r') -> IntL (l' + r')
-- --   (Mul, IntL l', IntL r') -> IntL (l' * r')
-- --   (Sub, IntL l', IntL r') -> IntL (l' - r')
-- --   (Div, IntL l', IntL r') | r' /= 0 -> IntL (l' `div` r')
-- --   (b', l', r') -> BinExpr b' l' r'
-- -- foldExprConstants e = e

foldExprConstants = descendE foldExprConstants'

foldExprConstants' :: Expr -> Expr
foldExprConstants' (BinExpr b l r) = case (b, foldExprConstants l, foldExprConstants r) of
  (Add, IntL l', IntL r')           -> IntL (l' + r')
  (Mul, IntL l', IntL r')           -> IntL (l' * r')
  (Sub, IntL l', IntL r')           -> IntL (l' - r')
  (Div, IntL l', IntL r') | r' /= 0 -> IntL (l' `div` r')
  (b', l', r')                      -> BinExpr b' l' r'
foldExprConstants' e = e

-- | look for var declarations and split them into two parts, a variable declaration followed by a assignment
deinitVariables :: Stmt -> Stmt
deinitVariables v@(VarStmt _ Nothing) = v
deinitVariables (VarStmt i (Just e)) = BlockStmt [VarStmt i Nothing, ExprStmt $ AssignExpr i e]
deinitVariables (BlockStmt ss) = BlockStmt $ map deinitVariables ss
deinitVariables (IfStmt c i e) = IfStmt c (deinitVariables i) (deinitVariables <$> e)
deinitVariables (WhileStmt e s) = WhileStmt e (deinitVariables s)
deinitVariables (FuncStmt i vs s) = FuncStmt i vs (deinitVariables s)
deinitVariables other = other

-- if we have a block stmt within a block stmt then flatten out.
deblock :: Stmt -> Stmt
deblock (IfStmt c t e) = IfStmt c (deblock t) (deblock <$> e)
deblock (WhileStmt c t) = WhileStmt c (deblock t)
deblock (FuncStmt n vs s) = FuncStmt n vs (deblock s)
deblock (BlockStmt ss) = BlockStmt $ reverse $ go ss []
  where
    go [] acc                       = acc
    go ((BlockStmt ss') : rest) acc = go rest (reverse ss' ++ acc)
    go (s' : rest) acc              = go rest (deblock s' : acc)
deblock s = s

data SimplifierError = Nil
  deriving (Show)

simplifer :: Stmt -> Either SimplifierError Stmt
simplifer = Right <$> (deblock . deinitVariables . foldConstants)
