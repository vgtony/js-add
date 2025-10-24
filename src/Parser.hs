{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Parser
Description : Core parser and evaluator for JavaScript-style expressions
Copyright   : (c) 2025
License     : BSD3

Core functionality for parsing and evaluating JavaScript-style arithmetic expressions.
This module is shared between CLI and GUI interfaces.
-}

module Parser 
  ( Stmt(..)
  , Expr(..) 
  , Environment
  , Name
  , evalExpr
  , pTop
  , pProgram
  , ppExpr
  , Parser
  ) where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Map                   as Map
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, eof, many, optional, try, (<|>), between)
import           Text.Megaparsec.Char       (alphaNumChar, char, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Monad.Combinators.Expr (Operator(..), makeExprParser)

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
keywordLet, keywordConst :: Text
keywordLet = "let"
keywordConst = "const"

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

-- | Parse numeric literals (supporting both integers and decimals)
pNumber :: Parser Expr
pNumber = ENum <$> lexeme (try L.float <|> fromIntegral <$> L.decimal)

-- | Parse parenthesized expressions
pParens :: Parser Expr
pParens = between (symbol "(") (symbol ")") pExpr

-- | Parse atomic expressions (variables, numbers, parentheses)
pAtom :: Parser Expr
pAtom = try pNumber <|> pVar <|> pParens

-- | Expression parser with operator precedence
pExpr :: Parser Expr
pExpr = makeExprParser pAtom operatorTable

-- | Operator precedence table (following mathematical conventions)
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixL (EMul <$ symbol opMul)
    , InfixL (EDiv <$ symbol opDiv)
    ]
  , [ InfixL (EAdd <$ symbol opAdd)
    , InfixL (ESub <$ symbol opSub)
    ]
  ]

-- | Statement parsers

-- | Parse let declarations: "let x = 5;"
pLet :: Parser Stmt
pLet = do
  _ <- symbol keywordLet
  name <- identifier
  _ <- symbol opAssign
  expr <- pExpr
  _ <- optional (symbol opSemi)
  pure (SLet name expr)

-- | Parse const declarations: "const pi = 3.14;"
pConst :: Parser Stmt
pConst = do
  _ <- symbol keywordConst
  name <- identifier
  _ <- symbol opAssign
  expr <- pExpr
  _ <- optional (symbol opSemi)
  pure (SConst name expr)

-- | Parse regular assignments: "x = 5;"
pAssign :: Parser Stmt
pAssign = do
  name <- identifier
  _ <- symbol opAssign
  expr <- pExpr
  _ <- optional (symbol opSemi)
  pure (SAssign name expr)

-- | Parse any statement
pStatement :: Parser Stmt
pStatement = try pLet <|> try pConst <|> pAssign

-- | Top-level parser (statement + EOF)
pTop :: Parser Stmt
pTop = do
  sc  -- consume leading whitespace
  stmt <- pStatement
  eof
  pure stmt

-- | Parse multiple statements (for multi-line code blocks)
pProgram :: Parser [Stmt]
pProgram = do
  sc  -- consume leading whitespace
  stmts <- pBlock <|> many pStatement
  eof
  pure stmts

-- | Parse block with curly braces
pBlock :: Parser [Stmt]
pBlock = between (symbol "{") (symbol "}") (many pStatement)

--------------------------------------------------------------------------------
-- | Expression evaluation

-- | Evaluate an expression in the given environment
-- Returns Nothing if evaluation fails (e.g., undefined variable, division by zero)
evalExpr :: Environment -> Expr -> Maybe Double
evalExpr env expr = case expr of
  EVar name -> Map.lookup name env
  ENum n    -> Just n
  EAdd a b  -> (+) <$> evalExpr env a <*> evalExpr env b
  ESub a b  -> (-) <$> evalExpr env a <*> evalExpr env b
  EMul a b  -> (*) <$> evalExpr env a <*> evalExpr env b
  EDiv a b  -> do
    x <- evalExpr env a
    y <- evalExpr env b
    if y == 0 then Nothing else Just (x / y)

-- | Pretty-print expressions for display
ppExpr :: Expr -> Text
ppExpr expr = case expr of
  EVar name -> name
  ENum n    -> T.pack (show n)
  EAdd a b  -> "(" <> ppExpr a <> " + " <> ppExpr b <> ")"
  ESub a b  -> "(" <> ppExpr a <> " - " <> ppExpr b <> ")"
  EMul a b  -> "(" <> ppExpr a <> " * " <> ppExpr b <> ")"
  EDiv a b  -> "(" <> ppExpr a <> " / " <> ppExpr b <> ")"