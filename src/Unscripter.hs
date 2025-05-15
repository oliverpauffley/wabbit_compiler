-- | Unscripter turns a program without a main function into a program with a main function
-- it does this by moving everything that isn't a function or global variable into a "main" function
-- that the unscript process creates
module Unscripter where

import           Expr

-- | Take the top level block statement and work through it with "unscript'".
unscript :: Stmt -> Stmt
unscript (BlockStmt ss) =
  let (globalBlock, mainBlock) = unscript' ([], []) ss
   in BlockStmt (reverse globalBlock ++ [FuncStmt "main" [] (BlockStmt $ reverse mainBlock)])
unscript s = s

-- | For each element of the top block, either add to a block or add to our main func block.
unscript' :: ([Stmt], [Stmt]) -> [Stmt] -> ([Stmt], [Stmt])
unscript' (acc, mainAcc) []                    = (acc, mainAcc)
unscript' (acc, mainAcc) (s@FuncStmt {} : ss)  = unscript' (s : acc, mainAcc) ss
unscript' (acc, mainAcc) (s@GlobalVar {} : ss) = unscript' (s : acc, mainAcc) ss
unscript' (acc, mainAcc) (s : ss)              = unscript' (acc, s : mainAcc) ss

data UnscriptError = Nil
  deriving (Show)

unscripter :: Stmt -> Either UnscriptError Stmt
unscripter = Right <$> unscript
