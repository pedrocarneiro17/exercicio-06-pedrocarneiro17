{
module L.L1.Frontend.HappyParser (runHappyParser) where
import L.L1.Frontend.Lexer
import L.L1.Frontend.Syntax
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
      TTimes      { Token _ TTimes }
      TDiv        { Token _ TDiv }
      TEOF        { Token _ TEOF }

%right TAssign
%left TPlus
%left TTimes TDiv
%left TSemicolon

%%

Program 
  : StmtList TEOF { L1 $1 }

StmtList
  : {- vazio -} { [] }
  | Stmt StmtList { $1 : $2 }

Stmt
  : TIdent TAssign Expr TSemicolon
      { LAssign (Var $1) $3 }
  | TRead TLParen TString TComma TIdent TRParen TSemicolon
      { LRead $3 (Var $5) }
  | TPrint TLParen Expr TRParen TSemicolon
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
  | Expr TTimes Expr
      { LMul $1 $3 }
  | Expr TDiv Expr
      { LDiv $1 $3 }
  | TLParen Expr TRParen
      { $2 }

{
parseError :: [Token] -> a
parseError [] = error "Erro de análise sintática: código vazio!"
parseError ts = error $ "Erro de análise sintática! Tokens restantes: " ++ show (map lexeme ts)

runHappyParser :: [Token] -> L1
runHappyParser = parseProgram
}