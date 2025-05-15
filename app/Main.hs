{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Blocks             as Blocker
import           Control.Monad      ((>=>))
import           Data.Bifunctor     (second)
import           Data.Text          hiding (map, replicate)
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           Expr
import qualified ExprStack
import qualified LLVM
import qualified Parser
import           Prettyprinter      (pretty)
import           PrintExpr          (showStmt)
import qualified Resolver
import qualified Returns
import           Simplifier         (deblock, deinitVariables, foldConstants,
                                     simplifer)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import qualified Tokenizer
import qualified Unscripter

data StageError = StageError Text Text

stageEither :: (Show e) => Text -> Either e a -> Either StageError a
stageEither name m = case m of
  Left e  -> Left (StageError name (T.pack $ show e))
  Right a -> Right a

printStageError :: StageError -> IO ()
printStageError (StageError name err) = do
  putStr (T.unpack name)
  putStrLn " Error:"
  putStr (T.unpack err)

data Stage i o = Stage
  { name     :: Text,
    runStage :: i -> Either StageError o
  }

instance Functor (Stage i) where
  fmap :: (a -> b) -> Stage i a -> Stage i b
  fmap f (Stage n r) = Stage n $ \i -> second f (r i)

makeStage :: (Show e) => Text -> (i -> Either e o) -> Stage i o
makeStage name r = Stage name (stageEither name . r)

printStage :: (Show b) => Stage a b -> a -> IO ()
printStage (Stage name r) a = case r a of
  Left err -> do
    printStageError err
    exitFailure
  Right b -> do
    putStrLn (T.unpack name <> ":")
    print b

lexerStage :: Stage Text [Tokenizer.Token]
lexerStage = makeStage "Lexer" Tokenizer.lexer

parserStage :: Stage [Tokenizer.Token] Stmt
parserStage = makeStage "Parser" Parser.parser

simplifierStage :: Stage Stmt Stmt
simplifierStage = makeStage "Simplifier" Simplifier.simplifer

resolverStage :: Stage Stmt Stmt
resolverStage = makeStage "Resolver" Resolver.resolver

unscriptStage :: Stage Stmt Stmt
unscriptStage = makeStage "Unscripter" Unscripter.unscripter

returnsStage :: Stage Stmt Stmt
returnsStage = makeStage "Returns" Returns.returner

exprStackStage :: Stage Stmt Stmt
exprStackStage = makeStage "ExprStack" ExprStack.exprStacker

blockerStage :: Stage Stmt Blocker.StackCode
blockerStage = makeStage "Blocker" Blocker.blocker

controlFlowStage :: Stage Blocker.StackCode Blocker.StackCode
controlFlowStage = makeStage "controlFlow" Blocker.controlFlower

convertLLVMStage :: Stage Blocker.StackCode LLVM.LLVMProgram
convertLLVMStage = makeStage "; llvm" LLVM.convertToLLVM

-- compose stages
(>->) :: Stage a b -> Stage b c -> Stage a c
(>->) (Stage _ r1) (Stage n2 r2) = Stage n2 (r1 >=> r2)

data Args = Args FilePath (Text -> IO ())

parseSimplify :: Stage Text Stmt
parseSimplify = lexerStage >-> parserStage >-> simplifierStage >-> resolverStage >-> unscriptStage >-> returnsStage

readStage :: Text -> Maybe (Text -> IO ())
readStage "lex" = Just $ printStage lexerStage
readStage "parse" = Just $ printStage (showStmt <$> (lexerStage >-> parserStage))
readStage "parseraw" = Just $ printStage (lexerStage >-> parserStage)
readStage "simplify" = Just $ printStage (showStmt <$> (lexerStage >-> parserStage >-> simplifierStage))
readStage "simplifyraw" = Just $ printStage (lexerStage >-> parserStage >-> simplifierStage)
readStage "resolve" = Just $ printStage (showStmt <$> (lexerStage >-> parserStage >-> simplifierStage >-> resolverStage))
readStage "unscript" = Just $ printStage (showStmt <$> (lexerStage >-> parserStage >-> simplifierStage >-> resolverStage >-> unscriptStage))
readStage "returns" = Just $ printStage (showStmt <$> parseSimplify)
readStage "exprstack" = Just $ printStage (showStmt <$> (parseSimplify >-> exprStackStage))
readStage "exprraw" = Just $ printStage (parseSimplify >-> exprStackStage)
readStage "blocker" = Just $ printStage (Blocker.showStack <$> (parseSimplify >-> exprStackStage >-> blockerStage))
readStage "controlflow" = Just $ printStage (Blocker.showStack <$> (parseSimplify >-> exprStackStage >-> blockerStage >-> controlFlowStage))
readStage "controlflowraw" = Just $ printStage (parseSimplify >-> exprStackStage >-> blockerStage >-> controlFlowStage)
readStage "llvm" = Just $ printStage (pretty <$> (parseSimplify >-> exprStackStage >-> blockerStage >-> controlFlowStage >-> convertLLVMStage))
readStage _ = Nothing

process :: Args -> IO ()
process (Args path stage) = do
  content <- TIO.readFile path
  stage content

parseArgs :: [String] -> Maybe Args
parseArgs (stageName : file : _) =
  Args file <$> readStage (T.toLower (T.pack stageName))
parseArgs _ = Nothing

main :: IO ()
main = do
  stringArgs <- getArgs
  case parseArgs stringArgs of
    Nothing   -> putStrLn "Unrecognized arguments"
    Just args -> process args
