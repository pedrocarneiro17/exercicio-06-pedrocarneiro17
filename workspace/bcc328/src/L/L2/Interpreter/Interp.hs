module L.L2.Interpreter.Interp (evalL2) where

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)

import L.L2.Frontend.Syntax
import Utils.Pretty
import Utils.Value
import Utils.Var

-- Ambientes: um para variáveis mutáveis, outro para imutáveis
type Env = Map Var Value
type ImmEnv = Map Var Value

-- Função principal para avaliar um programa L2
evalL2 :: [S2] -> IO (Either String (Env, ImmEnv))
evalL2 ss = foldM step (Right (Map.empty, Map.empty)) ss
  where
    step ac@(Left _) _ = pure ac
    step (Right (env, immEnv)) s2 = evalS2 env immEnv s2

-- Avalia um comando S2, atualizando os ambientes
evalS2 :: Env -> ImmEnv -> S2 -> IO (Either String (Env, ImmEnv))
evalS2 env immEnv (Def v e block) = do
  case evalE2 env immEnv e of
    Left err -> pure $ Left err
    Right val -> do
      let newImmEnv = Map.insert v val immEnv
      result <- evalL2' env newImmEnv block
      case result of
        Left err -> pure $ Left err
        Right (newEnv, _) -> pure $ Right (newEnv, immEnv)
evalS2 env immEnv (LRead s v) = do
  putStr s
  hFlush stdout
  val <- readValue
  pure $ Right (Map.insert v val env, immEnv)
evalS2 env immEnv (LPrint e) = do
  case evalE2 env immEnv e of
    Left err -> pure $ Left err
    Right val -> do
      putStrLn (showValue val)
      pure $ Right (env, immEnv)
evalS2 env immEnv (LAssign v e) = do
  case evalE2 env immEnv e of
    Left err -> pure $ Left err
    Right val -> pure $ Right (Map.insert v val env, immEnv)

-- Função auxiliar para avaliar um bloco dentro de Def
evalL2' :: Env -> ImmEnv -> [S2] -> IO (Either String (Env, ImmEnv))
evalL2' env immEnv block = foldM step (Right (env, immEnv)) block
  where
    step ac@(Left _) _ = pure ac
    step (Right (env', immEnv')) s2 = evalS2 env' immEnv' s2

-- Lê um valor da entrada padrão como string
readValue :: IO Value
readValue = VStr <$> getLine

-- Avalia uma expressão E2
evalE2 :: Env -> ImmEnv -> E2 -> Either String Value
evalE2 _ _ (LVal v) = Right v
evalE2 env immEnv (LVar v) = case Map.lookup v immEnv of
  Just val -> Right val
  Nothing -> case Map.lookup v env of
    Just val -> Right val
    Nothing -> Left ("Undefined variable: " ++ pretty v)
evalE2 env immEnv (LAdd e1 e2) = do
  v1 <- evalE2 env immEnv e1
  v2 <- evalE2 env immEnv e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VInt (n1 + n2)
    (VStr s1, VStr s2) -> Right $ VStr (s1 ++ s2)
    (VStr s, VInt n) -> Right $ VStr (s ++ show n)
    _ -> Left "Invalid addition"
evalE2 env immEnv (LMinus e1 e2) = do
  v1 <- evalE2 env immEnv e1
  v2 <- evalE2 env immEnv e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VInt (n1 - n2)
    _ -> Left "Invalid subtraction"
evalE2 env immEnv (LMul e1 e2) = do
  v1 <- evalE2 env immEnv e1
  v2 <- evalE2 env immEnv e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VInt (n1 * n2)
    _ -> Left "Invalid multiplication"
evalE2 env immEnv (LDiv e1 e2) = do
  v1 <- evalE2 env immEnv e1
  v2 <- evalE2 env immEnv e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> if n2 /= 0 then Right $ VInt (n1 `div` n2) else Left "Division by zero"
    _ -> Left "Invalid division"

-- Função para formatar valores para saída
showValue :: Value -> String
showValue (VInt n) = show n
showValue (VStr s) = s