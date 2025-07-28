{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module L.L2.Frontend.Lexer (Token (..), Lexeme (..), lexer) where

import Data.Char (isSpace)
}

%wrapper "posn"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$white = [\ \t\n\r]     -- whitespace

-- RE macros
@number     = (\-)? $digit+              -- números inteiros, incluindo negativos
@ident      = $alpha ($alpha | $digit)*  -- identificadores
@string     = \" ([^\"\\] | \\.)* \"     -- strings entre aspas duplas, com escapes

-- tokens declarations
tokens :-
  $white+         ;                    -- ignora espaços em branco
  "//" .*         ;                    -- ignora comentários de linha
  @string         {mkString}           -- strings entre aspas duplas
  @number         {mkNumber}           -- números inteiros
  @ident          {mkIdent}            -- identificadores
  "read"          {simpleToken TRead}  -- palavra reservada read
  "print"         {simpleToken TPrint} -- palavra reservada print
  "def"           {simpleToken TDef}   -- palavra reservada def
  "in"            {simpleToken TIn}    -- palavra reservada in
  "end"           {simpleToken TEnd}   -- palavra reservada end
  ":="            {simpleToken TAssign} -- atribuição
  ";"             {simpleToken TSemicolon} -- ponto e vírgula
  ","             {simpleToken TComma}  -- vírgula
  "("             {simpleToken TLParen} -- parêntese esquerdo
  ")"             {simpleToken TRParen} -- parêntese direito
  "+"             {simpleToken TPlus}   -- operador soma
  "-"             {simpleToken TMinus}  -- operador subtração
  "*"             {simpleToken TTimes}  -- operador multiplicação
  "/"             {simpleToken TDiv}    -- operador divisão
  <0> \0          {simpleToken TEOF}   -- fim de arquivo
  .               {mkError}            -- qualquer outro caractere é erro

{
data Token
  = Token {
      pos :: (Int, Int)
    , lexeme :: Lexeme
    } deriving (Eq, Ord, Show)

data Lexeme
  = TNumber Int        -- números inteiros
  | TIdent String      -- identificadores
  | TString String     -- literais de string
  | TRead              -- palavra reservada read
  | TPrint             -- palavra reservada print
  | TDef               -- palavra reservada def
  | TIn                -- palavra reservada in
  | TEnd               -- palavra reservada end
  | TAssign            -- atribuição (:=)
  | TSemicolon         -- ponto e vírgula
  | TComma             -- vírgula
  | TLParen            -- parêntese esquerdo
  | TRParen            -- parêntese direito
  | TPlus              -- operador +
  | TMinus             -- operador -
  | TTimes             -- operador *
  | TDiv               -- operador /
  | TError String      -- erro léxico
  | TEOF               -- fim de arquivo
  deriving (Eq, Ord, Show)

position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x, y)

mkNumber :: AlexPosn -> String -> Token
mkNumber p s = Token (position p) (TNumber $ read s)

mkIdent :: AlexPosn -> String -> Token
mkIdent p s = case s of
  "read"  -> Token (position p) TRead
  "print" -> Token (position p) TPrint
  "def"   -> Token (position p) TDef
  "in"    -> Token (position p) TIn
  "end"   -> Token (position p) TEnd
  _       -> Token (position p) (TIdent s)

mkString :: AlexPosn -> String -> Token
mkString p s = Token (position p) (TString $ unescape $ init $ drop 1 s) -- remove aspas e processa escapes

mkError :: AlexPosn -> String -> Token
mkError p s = Token (position p) (TError $ "caractere inválido: " ++ s)

simpleToken :: Lexeme -> AlexPosn -> String -> Token
simpleToken lx p _ = Token (position p) lx

-- Função para processar caracteres escapados
unescape :: String -> String
unescape [] = []
unescape ('\\' : c : cs) = case c of
  'n'  -> '\n' : unescape cs
  't'  -> '\t' : unescape cs
  '\\' -> '\\' : unescape cs
  '"'  -> '"'  : unescape cs
  _    -> c : unescape cs
unescape (c : cs) = c : unescape cs

lexer :: String -> [Token]
lexer input = case alexScanTokens input of
  [] -> [Token (1, 1) TEOF] -- Caso vazio, retorna TEOF
  ts -> if any (\t -> lexeme t == TEOF) ts
        then ts
        else ts ++ [Token (lastLine, lastCol + 1) TEOF]
        where
          lastToken = last ts
          (lastLine, lastCol) = pos lastToken
}