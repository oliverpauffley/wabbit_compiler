-- | stack machine instructions
module PrintExpr where

import           Expr
import           Prelude       hiding (EQ, LT)
import           Prettyprinter

showStmt :: Stmt -> Doc ann
showStmt (ExprStmt e) = showExpr e <> semi
showStmt (PrintStmt e) = group ("print" <+> showExpr e <> semi)
showStmt (VarStmt ident e) = group ("var" <+> pretty ident <> maybe mempty (\e' -> space <> "=" <+> showExpr e') e <> semi)
showStmt (GlobalVar ident) = group ("global_var" <+> pretty ident <> semi)
showStmt (LocalVar ident) = group ("local_var" <+> pretty ident <> semi)
showStmt (BlockStmt xs) = vsep $ map showStmt xs
showStmt (ReturnStmt e) = group ("return" <+> showExpr e <> semi)
showStmt (FuncStmt i vars s) =
  group
    ( "func"
        <+> pretty i
        <> parens (hsep (punctuate comma (map pretty vars)))
        <+> "{"
    )
    <> nest 4 (line <> showStmt s)
    <> line
    <> "}"
showStmt (IfStmt i t e) =
  "if"
    <+> showExpr i
    <+> "{"
    <+> vsep
      [ nest 4 (line <> showStmt t),
        "}" <> elseif e
      ]
  where
    elseif (Just elseExpr) = space <> "else" <+> "{" <> line <> indent 4 (showStmt elseExpr) <> line <> "}"
    elseif Nothing = mempty
showStmt (WhileStmt e s) =
  "while" <+> showExpr e <+> "{" <+> vsep [nest 4 (line <> showStmt s), "}"]

showExpr :: Expr -> Doc ann
showExpr (IntL l) = pretty (show l)
showExpr (Grouping e) = parens $ showExpr e
showExpr (BinExpr b e1 e2) = group (showExpr e1 <+> pretty (show b) <+> showExpr e2)
showExpr (NegExpr e) = "-" <> showExpr e
showExpr (AssignExpr ident e) = group (pretty ident <+> "=" <+> showExpr e)
showExpr (FunCall i e) = group (pretty i <> parens (hsep (punctuate comma (map showExpr e))))
showExpr (IdentExpr i) = pretty i
showExpr (PUSH v) = "PUSH" <> parens (pretty v)
showExpr ADD = "ADD"
showExpr MUL = "MUL"
showExpr LT = "LT"
showExpr EQ = "EQ"
showExpr (LOADGLOBAL n) = "LOAD_GLOBAL" <> parens (pretty n)
showExpr (LOADLOCAL n) = "LOAD_LOCAL" <> parens (pretty n)
showExpr (CALL name n) = "CALL" <> parens (hsep (punctuate comma [pretty name, pretty n]))
showExpr (INSTRUCTIONS xs) = brackets (hsep (punctuate comma (map showExpr xs)))
showExpr (STOREGLOBAL n) = "STORE_GLOBAL" <> parens (pretty n)
showExpr (STORELOCAL n) = "STORE_LOCAL" <> parens (pretty n)
showExpr PRINT = "PRINT"
showExpr RETURN = "RETURN"
showExpr (LOCAL n) = "LOCAL" <> parens (pretty n)
