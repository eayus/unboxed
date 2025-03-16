module Norm (eval, reify) where

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
    U n -> SU n
    Ur -> SUr
    Nat -> SNat
    Num n -> SNum n
    Ano t u -> SAno (e t) (e u)
    Let t u -> (b u) (e t)
    Box t -> SBox (e t)
    Bang t -> SBang (e t)
    New t -> SNew (evalGen r t)
    SigR t u -> SSigR (e t) (b u)
    Array t u -> SArray (e t) (e u)

evalGen :: Env -> Gen -> SGen
evalGen r = \case
  Pure t -> SPure $ eval r t
  Zeros t -> SZeros $ eval r t
  Pair t u -> SPair (eval r t) (evalGen r u)

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
    SU n -> U n
    SUr -> Ur
    SNat -> Nat
    SNum n -> Num n
    SAno t u -> Ano (e t) (e u)
    SLet t u -> Let (e t) (b u)
    SBox t -> Box (e t)
    SBang t -> Bang (e t)
    SNew t -> New (reifyGen k t)
    SSigR t u -> SigR (e t) (b u)
    SArray t u -> Array (e t) (e u)

reifyGen :: Int -> SGen -> Gen
reifyGen k = \case
  SPure t -> Pure $ reify k t
  SZeros t -> Zeros $ reify k t
  SPair t u -> Pair (reify k t) (reifyGen k u)