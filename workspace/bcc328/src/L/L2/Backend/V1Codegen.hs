module L.L2.Backend.V1Codegen (v1Codegen) where

import L.L2.Frontend.Syntax
import V.V1.Instr
import Utils.Value
import Utils.Var

-- Função principal para compilar um programa L2 para V1
v1Codegen :: L2 -> Code
v1Codegen (L2 ss) = concatMap compileS2 ss ++ [Halt]

-- Compila um comando S2 para instruções V1
compileS2 :: S2 -> Code
compileS2 (Def v e block) =
  -- Compila a expressão e armazena em v
  compileE2 e ++ [Store v] ++
  -- Compila o bloco com v disponível
  concatMap compileS2 block
  -- Não usa Pop, assume que V1 gerencia escopo via Store/Load
compileS2 (LAssign v e) = compileE2 e ++ [Store v]
compileS2 (LRead s v) = [Push (VStr s), Print, Input, Store v]
compileS2 (LPrint e) = compileE2 e ++ [Print]

-- Compila uma expressão E2 para instruções V1
compileE2 :: E2 -> Code
compileE2 (LVal v) = [Push v]
compileE2 (LVar v) = [Load v]
compileE2 (LAdd e1 e2) = compileE2 e1 ++ compileE2 e2 ++ [Add]
compileE2 (LMinus e1 e2) = compileE2 e1 ++ compileE2 e2 ++ [Sub]
compileE2 (LMul e1 e2) = compileE2 e1 ++ compileE2 e2 ++ [Mul]
compileE2 (LDiv e1 e2) = compileE2 e1 ++ compileE2 e2 ++ [Div]