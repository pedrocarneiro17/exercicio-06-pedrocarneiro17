module L.L1.Frontend.RecursiveParser (l1Parser, printL1Tree) where

import Control.Applicative (empty)
import L.L1.Frontend.Syntax
import Utils.Var (Var(..))
import Utils.Value (Value(..))
import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Bifunctor (first)


-- Lógica do Parser 

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

pIdentifier :: Parser Var
pIdentifier = Var <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

pStringLiteral :: Parser String
pStringLiteral = lexeme $ char '"' >> manyTill L.charLiteral (char '"')

pInteger :: Parser Int
pInteger = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

operatorTable :: [[Operator Parser E1]]
operatorTable =
  [ [ InfixL (LMul <$ symbol "*") ]
  , [ InfixL (LAdd <$ symbol "+"), InfixL (LMinus <$ symbol "-") ]
  ]

pTerm :: Parser E1
pTerm = choice
  [ parens pExpr
  , LVal . VInt <$> pInteger
  , LVal . VStr <$> pStringLiteral
  , LVar <$> pIdentifier
  ] <?> "expression term"

pExpr :: Parser E1
pExpr = makeExprParser pTerm operatorTable

pAssign :: Parser S1
pAssign = LAssign <$> pIdentifier <* symbol ":=" <*> pExpr <* symbol ";"

pRead :: Parser S1
pRead = do
  rword "read"
  parens (LRead <$> pStringLiteral <* symbol "," <*> pIdentifier) <* symbol ";"

pPrint :: Parser S1
pPrint = LPrint <$> (rword "print" *> parens pExpr) <* symbol ";"

pStmt :: Parser S1
pStmt = choice [pRead, pPrint, pAssign] <?> "statement"

pProgram :: Parser L1
pProgram = L1 <$> (sc *> many pStmt <* eof)

l1Parser :: String -> Either String L1
l1Parser input = first errorBundlePretty (runParser pProgram "" input)

-- Lógica de Impressão da Árvore

-- Função auxiliar para criar a indentação
indent :: Int -> String
indent n = replicate (n * 2) ' '

-- Função principal que inicia a impressão da árvore
printL1Tree :: L1 -> String
printL1Tree (L1 stmts) = "L1\n" ++ concatMap (pprStmt 1) stmts

-- Função para imprimir um comando (S1) com um nível de indentação
pprStmt :: Int -> S1 -> String
pprStmt level s = case s of
  LAssign var expr ->
    indent level ++ "LAssign\n" ++
    pprVar (level + 1) var ++
    pprExpr (level + 1) expr
  LRead prompt var ->
    indent level ++ "LRead \"" ++ prompt ++ "\"\n" ++
    pprVar (level + 1) var
  LPrint expr ->
    indent level ++ "LPrint\n" ++
    pprExpr (level + 1) expr

-- Função para imprimir uma expressão (E1)
pprExpr :: Int -> E1 -> String
pprExpr level e = case e of
  LAdd e1 e2 -> indent level ++ "LAdd\n" ++ pprExpr (level + 1) e1 ++ pprExpr (level + 1) e2
  LMinus e1 e2 -> indent level ++ "LMinus\n" ++ pprExpr (level + 1) e1 ++ pprExpr (level + 1) e2
  LMul e1 e2 -> indent level ++ "LMul\n" ++ pprExpr (level + 1) e1 ++ pprExpr (level + 1) e2
  LDiv e1 e2 -> indent level ++ "LDiv\n" ++ pprExpr (level + 1) e1 ++ pprExpr (level + 1) e2
  LVar var -> indent level ++ "LVar\n" ++ pprVar (level + 1) var
  LVal val -> indent level ++ "LVal\n" ++ pprValue (level + 1) val

-- Função para imprimir uma variável (Var)
pprVar :: Int -> Var -> String
pprVar level (Var v) = indent level ++ "Var \"" ++ v ++ "\"\n"

-- Função para imprimir um valor (Value)
pprValue :: Int -> Value -> String
pprValue level v = case v of
  VInt n  -> indent level ++ "VInt " ++ show n ++ "\n"
  VStr s  -> indent level ++ "VStr \"" ++ s ++ "\"\n"