module L.L2.Frontend.Semantic (checkSemantics) where

import L.L2.Frontend.Syntax
import Utils.Var
import Utils.Value
import qualified Data.Map as Map
import Control.Monad (unless)

-- Ambiente para variáveis imutáveis (mapeia Var para tipo ValueType)
type Env = Map.Map Var ValueType
data ValueType = IntType | StringType deriving (Eq, Show)

-- Verifica a semântica de um programa L2
checkSemantics :: [S2] -> Either String ()
checkSemantics stmts = do
  _ <- checkStmts Map.empty stmts
  return ()

-- Verifica uma lista de comandos em um ambiente
checkStmts :: Env -> [S2] -> Either String Env
checkStmts env [] = Right env
checkStmts env (s:ss) = do
  env' <- checkStmt env s
  checkStmts env' ss

-- Verifica um comando S2
checkStmt :: Env -> S2 -> Either String Env
checkStmt env (Def v e block) = do
  t <- checkExpr env e
  let env' = Map.insert v t env
  checkStmts env' block
checkStmt env (LAssign v e) = do
  t <- checkExpr env e
  -- Insere a variável no ambiente com o tipo da expressão
  let env' = Map.insert v t env
  return env'
checkStmt env (LRead _ v) = do
  -- read sempre armazena uma string
  return $ Map.insert v StringType env
checkStmt env (LPrint e) = do
  _ <- checkExpr env e
  return env

-- Verifica uma expressão E2
checkExpr :: Env -> E2 -> Either String ValueType
checkExpr env (LVal (VInt _)) = Right IntType
checkExpr env (LVal (VStr _)) = Right StringType
checkExpr env (LVar v) =
  case Map.lookup v env of
    Just t -> Right t
    Nothing -> Left $ "Variable " ++ show v ++ " not defined"
checkExpr env (LAdd e1 e2) = do
  t1 <- checkExpr env e1
  t2 <- checkExpr env e2
  case (t1, t2) of
    (IntType, IntType) -> Right IntType
    (StringType, StringType) -> Right StringType
    (StringType, IntType) -> Right StringType
    (IntType, StringType) -> Left "Invalid addition: int + string"
checkExpr env (LMinus e1 e2) = checkArithOp env e1 e2 "minus"
checkExpr env (LMul e1 e2) = checkArithOp env e1 e2 "multiplication"
checkExpr env (LDiv e1 e2) = checkArithOp env e1 e2 "division"

-- Verifica operações aritméticas (menos, multiplicação, divisão)
checkArithOp :: Env -> E2 -> E2 -> String -> Either String ValueType
checkArithOp env e1 e2 op = do
  t1 <- checkExpr env e1
  t2 <- checkExpr env e2
  unless (t1 == IntType && t2 == IntType) $ Left $ "Invalid " ++ op ++ ": operands must be integers"
  return IntType

-- Verifica compatibilidade de tipos para atribuição
compatibleTypes :: ValueType -> ValueType -> Bool
compatibleTypes IntType IntType = True
compatibleTypes StringType StringType = True
compatibleTypes StringType IntType = True -- Permite int em atribuição a string
compatibleTypes _ _ = False