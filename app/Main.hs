{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad (void)

-- AST

data Lambda
  = Var String
  | NumLit Int
  | Abs String Lambda
  | App Lambda Lambda
  deriving (Show, Eq)

-- PARSER

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser Lambda -> Parser Lambda
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = lexeme (try symbolIdent <|> alphaIdent)

-- operatori come + * - / ecc.
symbolIdent :: Parser String
symbolIdent = some (oneOf ("+-*/=<>" :: String))

-- identificatori classici: lettere e cifre
alphaIdent :: Parser String
alphaIdent = (:) <$> letterChar <*> many alphaNumChar


lambdaExpr :: Parser Lambda
lambdaExpr = do
  head <- term
  rest <- many term
  return $ foldl App head rest

term :: Parser Lambda
term =
      abstraction
  <|> parens lambdaExpr
  <|> NumLit <$> lexeme L.decimal
  <|> Var <$> identifier


abstraction :: Parser Lambda
abstraction = do
  _ <- symbol "\\"
  args <- some identifier
  _ <- optional (symbol ".")
  body <- abstraction <|> lambdaExpr
  return $ foldr Abs body args

parseLambdaExpr :: String -> Either String Lambda
parseLambdaExpr input =
  case parse (sc *> lambdaExpr <* eof) "" input of
    Left err -> Left (errorBundlePretty err)
    Right val -> Right val

--------------------------------------------------------------------------------
-- RENDERING AD ALBERO BINARIO
--------------------------------------------------------------------------------

-- Nodo per l'albero stampabile
data Tree = Node String Tree Tree | Leaf String | Empty
  deriving (Eq)

-- Costruzione dell'albero da un termine Lambda
buildTree :: Lambda -> Tree
buildTree (Var x)     = Leaf x
buildTree (NumLit n)  = Leaf (show n)
buildTree (Abs x b)   = Node ("\\" ++ x) (buildTree b) Empty
buildTree (App l r)   = Node "@" (buildTree l) (buildTree r)

-- Rendering ad albero indentato (ASCII)
renderTree :: Tree -> [String]
renderTree t = go t 0
  where
    go :: Tree -> Int -> [String]
    go Empty _ = []
    go (Leaf val) indent = [replicate indent ' ' ++ val]
    go (Node val left right) indent =
      let valLine = replicate indent ' ' ++ val
          leftLines = go left (indent + 4)
          rightLines = go right (indent + 4)
          connectorL = if left /= Empty then [replicate indent ' ' ++ "/"] else []
          connectorR = if right /= Empty then [replicate indent ' ' ++ "\\"] else []
      in [valLine]
         ++ connectorL ++ leftLines
         ++ connectorR ++ rightLines
-- PRETTY TREE RENDERER (tipo `tree` Unix)

renderPrettyTree :: Tree -> [String]
renderPrettyTree = go "" True
  where
    go :: String -> Bool -> Tree -> [String]
    go prefix isLast Empty = []
    go prefix isLast (Leaf val) =
      [prefix ++ (if isLast then "└── " else "├── ") ++ val]
    go prefix isLast (Node val left right) =
      let thisLine = prefix ++ (if isLast then "└── " else "├── ") ++ val
          newPrefix = prefix ++ (if isLast then "    " else "│   ")
          leftLines = go newPrefix (right == Empty) left
          rightLines = go newPrefix True right
      in [thisLine] ++ leftLines ++ rightLines

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Inserisci un'espressione lambda (es: \\func.\\arg.func arg):"
  input <- getLine
  case parseLambdaExpr input of
    Left err -> putStrLn $ "Errore di parsing:\n" ++ err
    Right ast -> do
        putStrLn "\nDiagramma pretty:\n"
        mapM_ putStrLn (renderPrettyTree (buildTree ast))