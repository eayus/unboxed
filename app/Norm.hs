module Norm (eval, reify, evalPtr) where

import Term

eval :: SEnv -> Tm -> Sm
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
    Bang t i -> SBang (e t) i
    New t -> SNew (evalGen r t)
    SigR t u i -> SSigR (e t) (b u) i
    Array t u i -> SArray (e t) (e u) i
    Index p i -> SIndex (evalPtr r p) i

evalGen :: SEnv -> Gen -> SGen
evalGen r = \case
  Pure t n -> SPure (eval r t) n
  Replicate t u n -> SReplicate (eval r t) (eval r u) n
  Pair t u i -> SPair (eval r t) (evalGen r u) i
  GAno t _ -> evalGen r t

evalPtr :: SEnv -> Ptr -> SPtr
evalPtr r = do
  let e = evalPtr r
  \case
    Fst p -> SFst (e p)
    Snd p -> SSnd (e p)
    Deref t -> SDeref (eval r t)
    FstR p -> SFstR (e p)
    SndR p i -> SSndR (e p) i
    Elem p t -> SElem (e p) (eval r t)

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
    SBang t i -> Bang (e t) i
    SNew t -> New (reifyGen k t)
    SSigR t u i -> SigR (e t) (b u) i
    SArray t u i -> Array (e t) (e u) i
    SIndex p i -> Index (reifyPtr k p) i

reifyGen :: Int -> SGen -> Gen
reifyGen k = \case
  SPure t n -> Pure (reify k t) n
  SReplicate t u n -> Replicate (reify k t) (reify k u) n
  SPair t u i -> Pair (reify k t) (reifyGen k u) i
  SGAno t u -> GAno (reifyGen k t) (reify k u)

reifyPtr :: Int -> SPtr -> Ptr
reifyPtr k = do
  let e = reifyPtr k
  \case
    SFst p -> Fst (e p)
    SSnd p -> Snd (e p)
    SDeref t -> Deref (reify k t)
    SFstR p -> FstR (e p)
    SSndR p i -> SndR (e p) i
    SElem p t -> Elem (e p) (reify k t)