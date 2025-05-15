-- | This module is just responsible for checking the function defintions within the
-- ast always have a final return. In cases where they don't we add a "return 0;"
module Returns where

import           Expr

-- | Check if all functions have return statements as their final statment.
returns :: Stmt -> Stmt
returns (FuncStmt n vars stmt) = FuncStmt n vars (maybeAddReturn stmt)
returns (BlockStmt ss)         = BlockStmt (map returns ss)
returns s                      = s

maybeAddReturn :: Stmt -> Stmt
maybeAddReturn (ReturnStmt e) = ReturnStmt e
maybeAddReturn (BlockStmt []) = BlockStmt [ReturnStmt (IntL 0)]
maybeAddReturn b@(BlockStmt ss) = case last ss of
  (ReturnStmt {}) -> b
  _s              -> BlockStmt $ ss <> [ReturnStmt (IntL 0)]
maybeAddReturn s = BlockStmt [s, ReturnStmt (IntL 0)]

data ReturnError = Nil
  deriving (Show)

returner :: Stmt -> Either ReturnError Stmt
returner = Right <$> returns
