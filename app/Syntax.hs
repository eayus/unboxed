module Syntax where

type Nm = String

data Tm
  = Var Nm
  | Lam Nm Tm
  | App Tm Tm
  | Pi Nm Tm Tm
  | U
  | Ano Tm Tm
  | Let Nm Tm Tm
  deriving Show