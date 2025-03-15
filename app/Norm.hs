module Norm where

import Term

eval :: Env -> Tm -> Sm
eval r = do
  let e = eval r
  let b t arg = eval (arg : r) t
  \case
    Var l -> reverse r !! l
    Lam t -> SLam $ b t
    App t u -> app (e t) (e u)
    Pi t u -> SPi (e t) (b u)
    U -> SU
    Ano t u -> SAno (e t) (e u)
    Let t u -> (b u) (e t)

app :: Sm -> Sm -> Sm
app = \case
  SLam f -> f
  t -> SApp t

reify :: Int -> Sm -> Tm
reify k = do
  let e = reify k
  let b t = reify (k + 1) $ t $ SVar k
  \case
    SVar l -> Var l
    SLam t -> Lam (b t)
    SApp t u -> App (e t) (e u)
    SPi t u -> Pi (e t) (b u)
    SU -> U
    SAno t u -> Ano (e t) (e u)
    SLet t u -> Let (e t) (b u)
