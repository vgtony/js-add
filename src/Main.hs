{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           System.IO
import           Control.Exception          (try, SomeException)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Text.Megaparsec            (runParser, errorBundlePretty)

import           Parser

-- | Simple multi-line editor
multiLineEditor :: IO Text
multiLineEditor = do
  putStrLn "Enter your JavaScript code (type 'END' on a line by itself to finish):"
  editLoop []
  where
    editLoop :: [Text] -> IO Text
    editLoop acc = do
      putStr "| "
      hFlush stdout
      line <- T.getLine
      if T.strip line == "END"
        then do
          let code = T.unlines (reverse acc)
          putStrLn "\nCode entered:"
          T.putStrLn code
          return code
        else editLoop (line : acc)

-- | Execute statements with environment
executeStatements :: [Stmt] -> Map Text Double -> IO (Map Text Double)
executeStatements [] env = return env
executeStatements (stmt:stmts) env = do
  newEnv <- executeStatement stmt env
  executeStatements stmts newEnv

-- | Execute a single statement
executeStatement :: Stmt -> Map Text Double -> IO (Map Text Double)
executeStatement stmt env = case stmt of
  SLet var expr -> do
    case evalExpr env expr of
      Just val -> do
        let newEnv = M.insert var val env
        putStrLn $ "let " ++ T.unpack var ++ " = " ++ show val
        return newEnv
      Nothing -> do
        putStrLn $ "Error evaluating expression in let statement"
        return env
  
  SConst var expr -> do
    case evalExpr env expr of
      Just val -> do
        let newEnv = M.insert var val env
        putStrLn $ "const " ++ T.unpack var ++ " = " ++ show val
        return newEnv
      Nothing -> do
        putStrLn $ "Error evaluating expression in const statement"
        return env
  
  SAssign var expr -> do
    case evalExpr env expr of
      Just val -> do
        let newEnv = M.insert var val env
        putStrLn $ T.unpack var ++ " = " ++ show val
        return newEnv
      Nothing -> do
        putStrLn $ "Error evaluating expression in assignment"
        return env

main :: IO ()
main = do
  putStrLn "JavaScript-style Calculator with Multi-line Editor"
  putStrLn "=================================================="
  code <- multiLineEditor
  
  case runParser pProgram "" code of
    Left parseError -> putStrLn $ "Parse error: " ++ errorBundlePretty parseError
    Right statements -> do
      putStrLn "Parsing successful!"
      putStrLn "Executing statements..."
      finalEnv <- executeStatements statements M.empty
      putStrLn "\nFinal environment:"
      mapM_ (\(var, val) -> putStrLn $ "  " ++ T.unpack var ++ " = " ++ show val) (M.toList finalEnv)
