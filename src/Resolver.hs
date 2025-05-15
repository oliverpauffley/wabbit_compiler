{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

-- | Resolve is responsible for taking the variables and assigning them either a global or local
-- tag depending on whereabouts they are in the environment.
module Resolver where

import           Control.Applicative      (liftA3)
import           Control.Monad.State.Lazy (MonadState (get), State, gets,
                                           modify, runState)
import           Data.Maybe               (fromJust)
import qualified Data.Set                 as S
import           Data.Text                (Text)
import           Expr
import           Prettyprinter            (Doc)
import           PrintExpr                (showStmt)

-- | we only care if variables are global or local not the depth or scope
data Scope = Global | Local

-- | Each environment contains a map of variable names to their scopes.
type ScopeVariables = S.Set Text

data Environment = Environment
  { _variables :: ScopeVariables,
    _scope     :: Scope,
    -- pointing up the tree to the "enclosing" env
    _enclosing :: Maybe Environment
  }

-- | Get a new empty environment. With Global state
initState :: Environment
initState = Environment S.empty Global Nothing

-- | Take the given environment and return a new one that is enclosed by the first.
-- Or alternatively think of this as creating a child environment of the given environment but it hands your back the child.
enclose :: Environment -> Environment
enclose r = Environment S.empty Local (Just r)

-- | Resolver is a state monad for traversing the AST to find variables to resolve.
newtype Resolver a = Resolver
  { _runResolver :: State Environment a
  }
  deriving (Functor, Applicative, Monad, MonadState Environment)

-- | Set the state to hold the given environment.
setEnv :: Environment -> Resolver ()
setEnv env = modify (const env)

-- | Modify the scope within the current environment.
changeScope :: Scope -> Resolver ()
changeScope s = modify (\r -> r {_scope = s})

-- | Insert a variable with the scope we are currently in.
insertVar :: Text -> Resolver ()
insertVar i = do
  Environment {..} <- get
  modify (\e -> e {_variables = S.insert i _variables})

-- | Find a variable in our environment or it's parent and return it's scope
-- crashes on missing variable
findVar :: Text -> Environment -> Maybe Scope
findVar i env = do
  let vars = _variables env
      enc = _enclosing env
      s = _scope env
  (if S.member i vars then Just s else enc >>= findVar i)

-- Adds a new child environment and sets the state to that. Does whatever action and then close the environment.
resolveInScope :: Resolver a -> Resolver a
resolveInScope toEval = do
  openEnv
  results <- toEval
  closeEnv
  return results
  where
    openEnv = get >>= (setEnv . enclose)
    closeEnv = gets _enclosing >>= setEnv . fromJust

-- | Traverse the AST and change all variable declarations and variable names to encode if they are global or local variables.
resolveScopes :: Stmt -> Stmt
resolveScopes stmt =
  let (stmt', _) = runState (_runResolver $ resolveScopes' stmt) initState
   in stmt'

resolveScopes' :: Stmt -> Resolver Stmt
resolveScopes' (ExprStmt e) = ExprStmt <$> resolveExprScopes e
resolveScopes' (PrintStmt e) = PrintStmt <$> resolveExprScopes e
resolveScopes' v@(VarStmt _ _) = convertScope v
resolveScopes' (BlockStmt vs) = BlockStmt <$> traverse resolveScopes' vs
resolveScopes' (IfStmt c i e) = liftA3 IfStmt (resolveExprScopes c) (resolveInScope (resolveScopes' i)) (traverse (resolveInScope . resolveScopes') e)
resolveScopes' (WhileStmt c e) = liftA2 WhileStmt (resolveExprScopes c) (resolveInScope (resolveScopes' e))
resolveScopes' (FuncStmt i vs s) =
  resolveInScope $ do
    mapM_ insertVar vs
    FuncStmt i vs <$> resolveScopes' s
resolveScopes' (ReturnStmt e) = ReturnStmt <$> resolveExprScopes e
resolveScopes' (GlobalVar _) = error "revisited node?"
resolveScopes' (LocalVar _) = error "revisited node?"

resolveExprScopes :: Expr -> Resolver Expr
resolveExprScopes (Grouping e) = Grouping <$> resolveExprScopes e
resolveExprScopes (BinExpr b l r) = liftA2 (BinExpr b) (resolveExprScopes l) (resolveExprScopes r)
resolveExprScopes (AssignExpr (Name i) e) = do
  env <- get
  case findVar i env of
    Nothing -> error $ "could not find var " ++ show i
    Just s -> case s of
      Local  -> AssignExpr (LocalName i) <$> resolveExprScopes e
      Global -> AssignExpr (GlobalName i) <$> resolveExprScopes e
resolveExprScopes (FunCall i e) = FunCall i <$> traverse resolveExprScopes e
resolveExprScopes i@(IntL _) = return i
resolveExprScopes (IdentExpr (Name i)) = do
  env <- get
  case findVar i env of
    Nothing -> error $ "could not find var" ++ show i
    Just s -> case s of
      Local  -> return $ IdentExpr (LocalName i)
      Global -> return $ IdentExpr (GlobalName i)
resolveExprScopes e = return e

-- | Turn a var stmt into a local or global stmt
convertScope :: Stmt -> Resolver Stmt
convertScope (VarStmt (Name i) Nothing) = do
  Environment {..} <- get
  insertVar i
  case _scope of
    Global -> return $ GlobalVar i
    Local  -> return $ LocalVar i
convertScope s = return s

data ResolverError = Nil
  deriving (Show)

resolver :: Stmt -> Either ResolverError Stmt
resolver = Right <$> resolveScopes
