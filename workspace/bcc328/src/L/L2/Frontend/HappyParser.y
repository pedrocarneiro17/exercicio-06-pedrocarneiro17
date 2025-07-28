{
module L.L2.Frontend.HappyParser (runL2Parser, printL2Tree) where
import L.L2.Frontend.Lexer
import L.L2.Frontend.Syntax
import Utils.Var
import Utils.Value
}

%name parseProgram
%tokentype { Token }
%error { parseError }

%token
      TIdent      { Token _ (TIdent $$) }
      TNumber     { Token _ (TNumber $$) }
      TString     { Token _ (TString $$) }
      TRead       { Token _ TRead }
      TPrint      { Token _ TPrint }
      TAssign     { Token _ TAssign }
      TSemicolon  { Token _ TSemicolon }
      TComma      { Token _ TComma }
      TLParen     { Token _ TLParen }
      TRParen     { Token _ TRParen }
      TPlus       { Token _ TPlus }
      TMinus      { Token _ TMinus }
      TTimes      { Token _ TTimes }
      TDiv        { Token _ TDiv }
      TDef        { Token _ TDef }
      TIn         { Token _ TIn }
      TEnd        { Token _ TEnd }
      TEOF        { Token _ TEOF }

%right TAssign
%left TPlus TMinus
%left TTimes TDiv
%left TSemicolon
%nonassoc TEnd

%%

Program 
  : StmtList TEOF             { $1 }
  | StmtList TEnd TEOF        { $1 }
  | StmtList TEnd TEnd TEOF   { $1 }

StmtList
  : {- vazio -}               { [] }
  | Stmt TSemicolon StmtList  { $1 : $3 }
  | TDef TIdent TAssign Expr TIn StmtList TEnd StmtList
      { Def (Var $2) $4 $6 : $8 }

Stmt
  : TIdent TAssign Expr
      { LAssign (Var $1) $3 }
  | TRead TLParen TString TComma TIdent TRParen
      { LRead $3 (Var $5) }
  | TPrint TLParen Expr TRParen
      { LPrint $3 }

Expr
  : TNumber
      { LVal (VInt $1) }
  | TIdent
      { LVar (Var $1) }
  | TString
      { LVal (VStr $1) }
  | Expr TPlus Expr
      { LAdd $1 $3 }
  | Expr TMinus Expr
      { LMinus $1 $3 }
  | Expr TTimes Expr
      { LMul $1 $3 }
  | Expr TDiv Expr
      { LDiv $1 $3 }
  | TLParen Expr TRParen
      { $2 }

{
parseError :: [Token] -> a
parseError [] = error "Erro de análise sintática: código vazio!"
parseError ts@(t:_) = error $ "Erro de análise sintática na posição " ++ show (pos t) ++ "! Tokens restantes: " ++ show (map lexeme ts)

runL2Parser :: [Token] -> [S2]
runL2Parser = parseProgram

-- Função auxiliar para criar a indentação
indent :: Int -> String
indent n = replicate (n * 2) ' '

-- Função principal que inicia a impressão da árvore
printL2Tree :: [S2] -> String
printL2Tree stmts = "L2\n" ++ concatMap (pprStmt 1) stmts

-- Função para imprimir um comando (S2) com um nível de indentação
pprStmt :: Int -> S2 -> String
pprStmt level s = case s of
  Def var expr block ->
    indent level ++ "Def\n" ++
    pprVar (level + 1) var ++
    pprExpr (level + 1) expr ++
    concatMap (pprStmt (level + 1)) block
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

-- Função para imprimir uma expressão (E2)
pprExpr :: Int -> E2 -> String
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
}
