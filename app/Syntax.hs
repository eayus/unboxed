module Syntax where

type Nm = String

data Tm
  = Var Nm
  | Lam Nm Tm
  | App Tm Tm
  | Pi Nm Tm Tm
  | U Int
  | Ur
  | Nat
  | Num Int
  | Ano Tm Tm
  | Let Nm Tm Tm
  | Box Tm
  | New Gen
  | Bang Tm -- Angle brackets in the agda formalization
  | SigR Nm Tm Tm
  | Array Tm Tm
  deriving Show

data Gen
  = Pure Tm
  | Zeros Tm -- TODO: Replace with replicate
  | Pair Tm Gen
  deriving Show