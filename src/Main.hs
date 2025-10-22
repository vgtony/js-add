{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : JavaScript-style calculator with expression parsing
Copyright   : (c) 2025
License     : BSD3
Maintainer  : example@example.com

A calculator that parses JavaScript-style variable declarations and arithmetic expressions.
Supports let/const declarations, variables, and basic arithmetic with proper operator precedence.
-}

module Main (main) where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Void                  (Void)
import qualified Data.Map                   as Map
import           Text.Megaparsec            (Parsec, eof, many, optional, runParser, try, (<|>), between)
import           Text.Megaparsec.Char       (alphaNumChar, char, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import           Control.Exception          (catch, IOException)
import           System.IO                  (hFlush, stdout)

--------------------------------------------------------------------------------
-- | AST (Abstract Syntax Tree)

-- | Variable names are represented as Text
type Name = Text

-- | Environment maps variable names to their numeric values
type Environment = Map.Map Name Double

-- | Mathematical expressions
data Expr
  = EVar Name       -- ^ Variable reference (e.g., x, foo)
  | ENum Double     -- ^ Numeric literal (e.g., 3.14, 42)
  | EAdd Expr Expr  -- ^ Addition (e.g., a + b)
  | ESub Expr Expr  -- ^ Subtraction (e.g., a - b)
  | EMul Expr Expr  -- ^ Multiplication (e.g., a * b)
  | EDiv Expr Expr  -- ^ Division (e.g., a / b)
  deriving (Eq, Show)

-- | JavaScript-style statements
data Stmt
  = SAssign Name Expr  -- ^ Regular assignment: x = 5
  | SLet Name Expr     -- ^ let declaration: let x = 5
  | SConst Name Expr   -- ^ const declaration: const x = 5
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Parser setup and constants

-- | Parser type alias for our specific use case
type Parser = Parsec Void Text

-- | Keywords and operators
keywordLet, keywordConst, keywordQuit :: Text
keywordLet = "let"
keywordConst = "const"
keywordQuit = "quit"

opAdd, opSub, opMul, opDiv, opAssign, opSemi :: Text
opAdd = "+"
opSub = "-"
opMul = "*"
opDiv = "/"
opAssign = "="
opSemi = ";"

-- | Comment styles
lineComment, blockCommentStart, blockCommentEnd :: Text
lineComment = "//"
blockCommentStart = "/*"
blockCommentEnd = "*/"

-- | Whitespace and comment parser
sc :: Parser ()
sc = L.space space1 (L.skipLineComment lineComment) (L.skipBlockComment blockCommentStart blockCommentEnd)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse identifiers following JavaScript rules
identifier :: Parser Name
identifier = lexeme . try $ do
  h <- letterChar
  t <- many (alphaNumChar <|> char '_')
  pure (T.pack (h:t))

-- | Basic expression parsers

-- | Parse variable references
pVar :: Parser Expr
pVar = EVar <$> identifier

-- | Parse numeric literals (integers and floats)
pNum :: Parser Expr
pNum = ENum <$> lexeme (try L.float <|> fromIntegral <$> (L.decimal :: Parser Integer))

-- | Parse parenthesized expressions
pParens :: Parser Expr
pParens = between (symbol "(") (symbol ")") pExpr

-- | Parse basic terms (atoms in expressions)
pTerm :: Parser Expr
pTerm = pNum <|> pVar <|> pParens

-- Expression parser with proper operator precedence
pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

-- | Operator precedence table (higher precedence first)
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixL (EMul <$ symbol opMul)
    , InfixL (EDiv <$ symbol opDiv)
    ]
  , [ InfixL (EAdd <$ symbol opAdd)
    , InfixL (ESub <$ symbol opSub)
    ]
  ]

-- | JavaScript-style statement parsers

-- | Parse let declarations: let x = expression;
pLet :: Parser Stmt
pLet = do
  _ <- symbol keywordLet
  name <- identifier
  _ <- symbol opAssign
  e <- pExpr
  _ <- optional (symbol opSemi)
  pure (SLet name e)

-- | Parse const declarations: const x = expression;
pConst :: Parser Stmt
pConst = do
  _ <- symbol keywordConst
  name <- identifier
  _ <- symbol opAssign
  e <- pExpr
  _ <- optional (symbol opSemi)
  pure (SConst name e)

-- | Parse regular assignments: x = expression;
pAssign :: Parser Stmt
pAssign = do
  name <- identifier
  _ <- symbol opAssign
  e <- pExpr
  _ <- optional (symbol opSemi)
  pure (SAssign name e)

-- | Parse any type of statement (tries let, const, then regular assignment)
pStatement :: Parser Stmt
pStatement = pLet <|> pConst <|> pAssign

-- | Parse a complete statement with whitespace handling and EOF
pTop :: Parser Stmt
pTop = sc *> pStatement <* eof

--------------------------------------------------------------------------------
-- | Pretty printing

-- | Convert expressions back to readable text
ppExpr :: Expr -> Text
ppExpr (EVar n)   = n
ppExpr (ENum i)   = T.pack (show i)
ppExpr (EAdd a b) = "(" <> ppExpr a <> " + " <> ppExpr b <> ")"
ppExpr (ESub a b) = "(" <> ppExpr a <> " - " <> ppExpr b <> ")"
ppExpr (EMul a b) = "(" <> ppExpr a <> " * " <> ppExpr b <> ")"
ppExpr (EDiv a b) = "(" <> ppExpr a <> " / " <> ppExpr b <> ")"

--------------------------------------------------------------------------------
-- | Expression evaluation

-- | Evaluate expressions in the given environment
-- Returns Nothing for division by zero or undefined variables
evalExpr :: Environment -> Expr -> Maybe Double
evalExpr _   (ENum i)   = Just i
evalExpr env (EVar n)   = Map.lookup n env
evalExpr env (EAdd a b) = (+) <$> evalExpr env a <*> evalExpr env b
evalExpr env (ESub a b) = (-) <$> evalExpr env a <*> evalExpr env b
evalExpr env (EMul a b) = (*) <$> evalExpr env a <*> evalExpr env b
evalExpr env (EDiv a b) = do
  va <- evalExpr env a
  vb <- evalExpr env b
  if vb == 0 then Nothing else Just (va / vb)

--------------------------------------------------------------------------------
-- | Statement execution and main program

-- | Execute a statement and return the updated environment
executeStmt :: Environment -> Stmt -> IO Environment
executeStmt env (SAssign name expr) = executeAssignment env "Assignment" name expr
executeStmt env (SLet name expr)    = executeAssignment env "Let declaration" name expr  
executeStmt env (SConst name expr)  = executeAssignment env "Const declaration" name expr

-- | Helper function to execute any type of assignment
executeAssignment :: Environment -> String -> Name -> Expr -> IO Environment
executeAssignment env stmtType name expr = do
  putStrLn (stmtType <> ": " <> T.unpack name <> " = " <> T.unpack (ppExpr expr))
  case evalExpr env expr of
    Just result -> do
      putStrLn ("Result: " <> T.unpack name <> " = " <> show result)
      pure (Map.insert name result env)
    Nothing -> do
      putStrLn "Error: Cannot evaluate expression (division by zero or undefined variable)"
      pure env

-- | Main entry point
main :: IO ()
main = do
  putStrLn "JavaScript-Style Calculator!"
  putStrLn "Supports: +, -, *, /, variables, decimals, and JavaScript syntax"
  putStrLn "Examples:"
  putStrLn ("  " <> T.unpack keywordLet <> " x = 10;")
  putStrLn ("  " <> T.unpack keywordLet <> " y = 20;")
  putStrLn "  z = x + y"
  putStrLn ("  " <> T.unpack keywordConst <> " pi = 3.14159;")
  putStrLn "  area = pi * 5 * 5"
  putStrLn "  y = x * 2 + 5"
  putStrLn "  total = (x + y) / 2"
  putStrLn ("Type '" <> T.unpack keywordQuit <> "' to exit\n")
  calculator Map.empty

-- | Interactive calculator loop
calculator :: Environment -> IO ()
calculator env = do
  putStr "calc> "
  hFlush stdout
  result <- tryGetLine
  case result of
    Nothing -> do
      putStrLn ""
      putStrLn "Session ended."
    Just line -> 
      if T.strip line == keywordQuit
        then putStrLn "Goodbye!"
        else do
          case runParser pTop "<stdin>" line of
            Left err -> do
              putStrLn ("Parse error: " <> show err)
              calculator env
            Right stmt -> do
              newEnv <- executeStmt env stmt
              putStrLn ""
              calculator newEnv

-- | Safe getLine that handles EOF gracefully
tryGetLine :: IO (Maybe Text)
tryGetLine = catch (Just <$> T.getLine) handleEOF
  where
    handleEOF :: IOException -> IO (Maybe Text)
    handleEOF _ = return Nothing
