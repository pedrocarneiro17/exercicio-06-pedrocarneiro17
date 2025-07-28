module V.V2.Instr where

import Utils.Value
import Utils.Var

type Code = [Instr]

data Instr
  = Push Value
  | Add
  | Mul
  | Sub
  | Div
  | Lt
  | IEq
  | And
  | Not
  | Cat
  | Size
  | I2S
  | I2B
  | B2S
  | B2I
  | S2I
  | S2B
  | Input
  | Print
  | Load Var
  | Store Var
  | Halt
  deriving (Eq, Ord, Show)