{-# LANGUAGE InstanceSigs #-}

module Expr where

import           Control.Applicative    (liftA3)
import           Control.Monad.Identity (runIdentity)
import           Data.Text              hiding (group)
import           Prelude                hiding (EQ, LT)
import           Prettyprinter          (Pretty (pretty))

data Stmt
  = ExprStmt Expr
  | PrintStmt Expr
  | VarStmt Identifier (Maybe Expr)
  | BlockStmt [Stmt]
  | IfStmt Expr Stmt (Maybe Stmt)
  | WhileStmt Expr Stmt
  | FuncStmt Text [Text] Stmt
  | ReturnStmt Expr
  | GlobalVar Text
  | LocalVar Text
  deriving (Show, Eq)

data Expr
  = Grouping Expr
  | BinExpr BinOp Expr Expr
  | NegExpr Expr
  | AssignExpr Identifier Expr
  | FunCall Text [Expr]
  | IntL Int
  | IdentExpr Identifier
  | -- stack machine instructions
    INSTRUCTIONS [Expr]
  | PUSH Int
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
  deriving (Show, Eq)

descendESM :: (Monad m, Applicative m) => (Expr -> m Expr) -> Stmt -> m Stmt
descendESM f (ExprStmt e) = ExprStmt <$> descendEM f e
descendESM f (PrintStmt e) = PrintStmt <$> descendEM f e
descendESM f (VarStmt i e) = VarStmt i <$> maybe (pure Nothing) (fmap Just . descendEM f) e
descendESM f (BlockStmt bs) = BlockStmt <$> traverse (descendESM f) bs
descendESM f (IfStmt i t e) = IfStmt <$> descendEM f i <*> descendESM f t <*> traverse (descendESM f) e
descendESM f (WhileStmt i t) = undefined
descendESM f (FuncStmt n vars b) = undefined
descendESM f (ReturnStmt s) = undefined
descendESM f (GlobalVar s) = undefined
descendESM f (LocalVar s) = undefined

descendEM :: (Monad m, Applicative m) => (Expr -> m Expr) -> Expr -> m Expr
descendEM f e = case e of
  Grouping e'     -> Grouping <$> descendEM f e'
  BinExpr op l r  -> BinExpr op <$> descendEM f l <*> descendEM f r
  NegExpr e'      -> NegExpr <$> descendEM f e'
  AssignExpr i e' -> AssignExpr i <$> descendEM f e'
  FunCall t e'    -> FunCall t <$> traverse (descendEM f) e'
  IntL i          -> pure $ IntL i
  IdentExpr i     -> pure $ IdentExpr i
  INSTRUCTIONS e' -> INSTRUCTIONS <$> traverse (descendEM f) e'
  PUSH i          -> pure $ PUSH i
  ADD             -> pure ADD
  MUL             -> pure MUL
  LT              -> pure LT
  EQ              -> pure EQ
  LOADGLOBAL t    -> pure $ LOADGLOBAL t
  LOADLOCAL t     -> pure $ LOADLOCAL t
  STOREGLOBAL t   -> pure $ STOREGLOBAL t
  STORELOCAL t    -> pure $ STORELOCAL t
  CALL t i        -> pure $ CALL t i
  PRINT           -> pure PRINT
  RETURN          -> pure RETURN
  LOCAL t         -> pure $ LOCAL t

descendE :: (Expr -> Expr) -> Expr -> Expr
descendE f e = runIdentity (descendEM (return . f) e)

data Identifier = Name Text | LocalName Text | GlobalName Text
  deriving (Ord, Eq)

instance Show Identifier where
  show :: Identifier -> String
  show (Name t)       = unpack t
  show (LocalName t)  = "L:" <> unpack t
  show (GlobalName t) = "G:" <> unpack t

instance Pretty Identifier where
  pretty i = pretty $ show i

data BinOp
  = Add
  | Mul
  | Sub
  | Div
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | Eq
  | Neq
  deriving (Eq)

instance Show BinOp where
  show Add              = "+"
  show Mul              = "*"
  show Sub              = "-"
  show Div              = "/"
  show LessThan         = "<"
  show LessThanEqual    = "<="
  show GreaterThan      = ">"
  show GreaterThanEqual = ">="
  show Eq               = "=="
  show Neq              = "!="
