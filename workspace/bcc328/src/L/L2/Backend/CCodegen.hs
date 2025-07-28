module L.L2.Backend.CCodegen (cCodegen) where

import L.L2.Frontend.Syntax
import Utils.Var (Var(..))
import Utils.Value (Value(..))
import Data.List (unlines)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Tipo para representar tipos de valores
data ValueType = IntType | StringType deriving (Eq, Show)

-- Extrai a string interna de Var para nomes válidos em C
showVar :: Var -> String
showVar (Var s) = "v_" ++ s

-- Gera código C para um programa L2
cCodegen :: L2 -> String
cCodegen (L2 stmts) =
  unlines
    [ "#include <stdio.h>"
    , "#include <stdlib.h>"
    , "#include <string.h>"
    , ""
    , "int main() {"
    , indent 2 "static char buf[2048];"
    , indent 2 (genStmts' Map.empty stmts)
    , indent 2 "return 0;"
    , "}"
    ]
  where
    genStmts' env stmts = 
      let (_, code) = genStmts env stmts
      in code

-- Indenta um bloco de código
indent :: Int -> String -> String
indent n s = unlines $ map (replicate n ' ' ++) (lines s)

-- Gera código C para uma lista de comandos
genStmts :: Map.Map Var ValueType -> [S2] -> (Map.Map Var ValueType, String)
genStmts env stmts =
  foldl (\(env', code) stmt -> 
    let (newEnv, newCode) = genStmt env' stmt
    in (newEnv, code ++ newCode)
  ) (env, "") stmts

-- Gera código C para um comando
genStmt :: Map.Map Var ValueType -> S2 -> (Map.Map Var ValueType, String)
genStmt env (Def v e block) =
  let varType = evalType env e
      newEnv = Map.insert v varType env
      decl = case varType of
               IntType -> "const int " ++ showVar v ++ " = " ++ genExpr env e ++ ";\n"
               StringType -> "const char " ++ showVar v ++ "[1024] = " ++ genExpr env e ++ ";\n"
      (_, blockCode) = genStmts newEnv block
  in (env, "{\n" ++ indent 2 decl ++ indent 2 blockCode ++ "}\n")

genStmt env (LAssign v e) =
  let varType = evalType env e
      newEnv = Map.insert v varType env
      code = case varType of
               IntType -> showVar v ++ " = " ++ genExpr env e ++ ";\n"
               StringType -> "strcpy(" ++ showVar v ++ ", " ++ genExpr env e ++ ");\n"
  in (newEnv, code)

genStmt env (LRead prompt v) =
  let newEnv = Map.insert v StringType env
      code = "printf(" ++ genExpr env (LVal (VStr prompt)) ++ ");\n" ++
             "scanf(\"%1023s\", " ++ showVar v ++ ");\n"
  in (newEnv, code)

genStmt env (LPrint e) =
  let code = case evalType env e of
               IntType -> "printf(\"%d\\n\", " ++ genExpr env e ++ ");\n"
               StringType -> "printf(\"%s\\n\", " ++ genExpr env e ++ ");\n"
  in (env, code)

-- Gera código C para uma expressão
genExpr :: Map.Map Var ValueType -> E2 -> String
genExpr _ (LVal (VInt n)) = show n
genExpr _ (LVal (VStr s)) = "\"" ++ escapeString s ++ "\""
genExpr env (LVar v) = case Map.lookup v env of
  Just _ -> showVar v
  Nothing -> error $ "Variable " ++ showVar v ++ " not defined"
genExpr env (LAdd e1 e2) =
  case (evalType env e1, evalType env e2) of
    (IntType, IntType) -> "(" ++ genExpr env e1 ++ " + " ++ genExpr env e2 ++ ")"
    (StringType, StringType) -> "strcat(strcpy(buf, " ++ genExpr env e1 ++ "), " ++ genExpr env e2 ++ ")"
    _ -> error "Invalid addition"
genExpr env (LMinus e1 e2) = "(" ++ genExpr env e1 ++ " - " ++ genExpr env e2 ++ ")"
genExpr env (LMul e1 e2) = "(" ++ genExpr env e1 ++ " * " ++ genExpr env e2 ++ ")"
genExpr env (LDiv e1 e2) = "(" ++ genExpr env e1 ++ " / " ++ genExpr env e2 ++ ")"

-- Determina o tipo de uma expressão
evalType :: Map.Map Var ValueType -> E2 -> ValueType
evalType _ (LVal (VInt _)) = IntType
evalType _ (LVal (VStr _)) = StringType
evalType env (LVar v) = case Map.lookup v env of
  Just t -> t
  Nothing -> error $ "Variable " ++ showVar v ++ " not defined"
evalType env (LAdd e1 e2) = case (evalType env e1, evalType env e2) of
  (IntType, IntType) -> IntType
  (StringType, StringType) -> StringType
  _ -> error "Invalid addition"
evalType env (LMinus _ _) = IntType
evalType env (LMul _ _) = IntType
evalType env (LDiv _ _) = IntType

-- Escapa caracteres especiais em strings
escapeString :: String -> String
escapeString s = concatMap escapeChar s
  where
    escapeChar c = case c of
      '\n' -> "\\n"
      '"' -> "\\\""
      '\\' -> "\\\\"
      _ -> [c]