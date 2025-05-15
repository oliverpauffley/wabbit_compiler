{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Expr             hiding (EQ, LT, MUL, PRINT, RETURN)
import           Parser
import           Prelude          hiding (EQ, LT)
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec (testSpecs)
import           Tokenizer

main :: IO ()
main = do
  parse <- concat <$> mapM testSpecs [parseStmtTests, parseExprTests]
  defaultMain $
    testGroup
      "All Tests"
      [ testGroup "Parsing" parse
      ]

parseStmtTests :: Spec
parseStmtTests =
  describe "parse tokens into statement ast" $ do
    it "handles print statements" $
      parser [PRINT, INTEGER 1, SEMI] `shouldBe` inBlock (PrintStmt (IntL 1))
    it "handles print statements with string variable" $
      parser [PRINT, NAME "y", SEMI] `shouldBe` inBlock (PrintStmt (IdentExpr (Name "y")))
    it "parses var stmts" $
      parser [VAR, NAME "y", SEMI] `shouldBe` inBlock (VarStmt (Name "y") Nothing)
    it "parses var stmts with expression" $
      parser [VAR, NAME "y", ASSIGN, INTEGER 10, SEMI] `shouldBe` inBlock (VarStmt (Name "y") (Just $ IntL 10))
    it "parses blocks of statements" $
      parser [PRINT, NAME "y", SEMI, VAR, NAME "x", SEMI]
        `shouldBe` inBlock (BlockStmt [PrintStmt (IdentExpr (Name "y")), VarStmt (Name "x") Nothing])
    it "parses if statements with else" $
      parser [IF, INTEGER 10, LT, INTEGER 12, LBRACE, PRINT, INTEGER 1, SEMI, RBRACE, ELSE, LBRACE, PRINT, INTEGER 0, SEMI, RBRACE]
        `shouldBe` inBlock (IfStmt (BinExpr LessThan (IntL 10) (IntL 12)) (BlockStmt [PrintStmt (IntL 1)]) (Just $ BlockStmt [PrintStmt (IntL 0)]))
    it "parses if statements without else" $
      parser [IF, INTEGER 10, LT, INTEGER 12, LBRACE, PRINT, INTEGER 1, SEMI, RBRACE]
        `shouldBe` inBlock (IfStmt (BinExpr LessThan (IntL 10) (IntL 12)) (BlockStmt [PrintStmt (IntL 1)]) Nothing)
    it "parses while statements" $
      parser [WHILE, INTEGER 10, EQ, INTEGER 10, LBRACE, PRINT, NAME "y", SEMI, RBRACE]
        `shouldBe` inBlock (WhileStmt (BinExpr Eq (IntL 10) (IntL 10)) (BlockStmt [PrintStmt (IdentExpr (Name "y"))]))
    it "parses function definitions" $
      parser [FUNC, NAME "hello", LPAREN, RPAREN, LBRACE, PRINT, INTEGER 10, SEMI, RBRACE]
        `shouldBe` inBlock (FuncStmt "hello" [] (BlockStmt [PrintStmt (IntL 10)]))
    it "parses function definitions with input variables" $
      parser [FUNC, NAME "hello", LPAREN, NAME "x", RPAREN, LBRACE, PRINT, NAME "x", SEMI, RBRACE]
        `shouldBe` inBlock (FuncStmt "hello" ["x"] (BlockStmt [PrintStmt (IdentExpr (Name "x"))]))
    it "parses return statements" $
      parser [RETURN, NAME "x", SEMI]
        `shouldBe` inBlock (ReturnStmt (IdentExpr (Name "x")))
    it "handles nested calls" $
      parser [RETURN,NAME "x",MUL,NAME "factre",LPAREN,NAME "x",PLUS,INTEGER 1,COMMA,NAME "n",RPAREN,SEMI]
        `shouldBe` inBlock (ReturnStmt (BinExpr Mul (IdentExpr (Name"x")) (FunCall "factre" [BinExpr Add (IdentExpr $ Name "x") (IntL 1), IdentExpr (Name "n") ])) )

parseExprTests :: Spec
parseExprTests =
  describe "parse tokens into expressions ast" $ do
    it "handles literal expressions" $
      parser [INTEGER 10, SEMI] `shouldBe` inBlock (ExprStmt $ IntL 10)
    it "handles bin expressions - less than" $
      parser [INTEGER 10, LT, INTEGER 12, SEMI] `shouldBe` inBlock (ExprStmt $ BinExpr LessThan (IntL 10) (IntL 12))
    it "handles grouping expressions" $
      parser [LPAREN, INTEGER 10, LT, INTEGER 12, RPAREN, SEMI] `shouldBe` inBlock (ExprStmt $ Grouping (BinExpr LessThan (IntL 10) (IntL 12)))
    it "handles assignment expressions" $
      parser [NAME "x", ASSIGN, NAME "x", PLUS, INTEGER 10, SEMI] `shouldBe` inBlock (ExprStmt $ AssignExpr (Name "x") (BinExpr Add (IdentExpr (Name "x")) (IntL 10)))
    it "handles function calls" $
      parser [NAME "factorial", LPAREN, NAME "x", RPAREN, SEMI]
        `shouldBe` inBlock (ExprStmt $ FunCall "factorial" [IdentExpr $ Name "x"])

-- since we always parse as a block stmt put the given stmt in a block for testing
inBlock :: Stmt -> Either a Stmt
inBlock b@(BlockStmt _) = Right b
inBlock s               = Right (BlockStmt [s])
