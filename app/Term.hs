module Term where

data Tm
  = Var Int
  | Lam Tm
  | App Tm Tm
  | Pi Tm Tm
  | U
  | Ano Tm Tm
  | Let Tm Tm
  deriving (Eq, Show)

data Sm
  = SVar Int -- de Bruijn level
  | SLam (Sm -> Sm)
  | SApp Sm Sm
  | SPi Sm (Sm -> Sm)
  | SU
  | SAno Sm Sm
  | SLet Sm (Sm -> Sm)

type Env = [Sm]