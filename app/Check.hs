module Check where

import Control.Monad
import Control.Monad.Error.Class
import Norm
import Syntax qualified as S
import Term qualified as T

-- (Name, (Type, Value))
type Ctxt = [(S.Nm, (T.Sm, T.Sm))]

infer :: MonadError String m => Ctxt -> S.Tm -> m (T.Sm, T.Tm)
infer c = \case
  S.Var x -> lkp x c
  S.Lam {} -> throwError "Can't infer lam"
  S.App t u -> infer c t >>= \case
    (T.SPi a b, t') -> do
      u' <- check c u a
      pure (b (eval (env c) u'), T.App t' u')
    _ -> throwError "Can only apply functions"
  S.Pi x t u -> do
    (_, t') <- checkU c t
    let st = eval (env c) t'
    (_, u') <- checkU ((x, (st, T.SVar (length c))) : c) u
    pure (T.SU 8, T.Pi t' u')
  S.U n -> pure (T.SU 0, T.U n)
  S.Ur -> pure (T.SU 0, T.Ur)
  S.Nat -> pure (T.SU 0, T.Nat)
  S.Num n -> pure (T.SNat, T.Num n)
  S.Ano t u -> do
    (_, u') <- checkU c u
    let su = (eval (env c) u')
    t' <- check c t su
    pure (su, T.Ano t' u')
  S.Let x t u -> do
    (a, t') <- infer c t
    let st = eval (env c) t'
    (b, u') <- infer ((x, (a, st)) : c) u
    pure (b, T.Let t' u')
  S.Box t -> do
    t' <- check c t T.SUr
    pure (T.SU 8, T.Box t')
  S.Bang t -> do
    (i, t') <- checkU c t
    pure (T.SUr, T.Bang t' i)
  S.New t -> do
    (a, t') <- inferGen c t
    pure (T.SBox a, T.New t')
  S.SigR x t u -> do
    (i, t') <- checkU c t
    let st = eval (env c) t'
    u' <- check ((x, (st, T.SVar (length c))) : c) u T.SUr
    pure (T.SUr, T.SigR t' u' i)
  S.Array t u -> do
    t' <- check c t T.SNat
    (i, u') <- checkU c u
    pure (T.SUr, T.Array t' u' i)
  S.Index p -> do
    (a, p') <- inferPtr c p
    case a of
      T.SBang b i -> pure (b, T.Index p' i)
      _ -> throwError $ "Cannot extract the unsized data of type '" ++ show (reify (length c) a) ++ "' using index.\nWe expect a type which is wrapped in '!'"

inferGen :: MonadError String m => Ctxt -> S.Gen -> m (T.Sm, T.Gen)
inferGen c = \case
  S.Pure t a -> do
    (i, a') <- checkU c a
    let sa = eval (env c) a'
    t' <- check c t sa
    pure (T.SBang sa i, T.Pure t' i)
  S.Replicate n t a -> do
    (i, a') <- checkU c a
    let sa = eval (env c) a'
    n' <- check c n T.SNat
    t' <- check c t sa
    let sn = eval (env c) n'
    pure (T.SArray sn sa i, T.Replicate n' t' i)
  S.Pair _ _ -> throwError "Cannot infer pair"
  S.GAno t a -> do
    a' <- check c a T.SUr
    let sa = eval (env c) a'
    t' <- checkGen c t sa
    pure (sa, T.GAno t' a')

checkGen :: MonadError String m => Ctxt -> S.Gen -> T.Sm -> m T.Gen
checkGen c (S.Pair t u) (T.SSigR a b i) = do
  t' <- check c t a
  let st = eval (env c) t'
  u' <- checkGen c u (b st)
  pure (T.Pair t' u' i)
checkGen c t@(S.Pair {}) a = throwError $ "The (generator) pair " ++ show t ++ " has a sigma type, but it is expected to have type " ++ show (reify (length c) a)
checkGen c t a = do
  (b, t') <- inferGen c t
  let k = length c
  let a'  = reify k a
  let b'  = reify k b
  unless (a' == b') (throwError $ "When checking the generator " ++ show t ++ "\nMismatch between\n" ++ show a' ++ "\n" ++ show b')
  pure t'

inferPtr :: MonadError String m => Ctxt -> S.Ptr -> m (T.Sm, T.Ptr)
inferPtr c = \case
  S.Fst _ -> undefined -- TODO: Implement sized sigmas
  S.Snd _ -> undefined
  S.Deref t -> infer c t >>= \case
    (T.SBox a, t') -> pure (a, T.Deref t')
    _ -> throwError "Can only deref boxed data"
  S.FstR p -> inferPtr c p >>= \case
    (T.SSigR a _ i, p') -> pure (T.SBang a i, T.FstR p')
    _ -> throwError "Can only 'fstr' a runtime sigma"
  S.SndR p -> inferPtr c p >>= \case
    (T.SSigR _ b i, p') -> do
      let sp = evalPtr (env c) p'
      pure (b (T.SIndex (T.SFstR sp) i), T.SndR p' i)
    _ -> throwError "Can only 'sndr' a runtime sigma"
  S.Elem p t -> inferPtr c p >>= \case
    (T.SArray _ a i, p') -> do
      t' <- check c t T.SNat
      pure (T.SBang a i, T.Elem p' t')
    _ -> throwError "Can only extract an element out of an array"

checkU :: MonadError String m => Ctxt -> S.Tm -> m (Int, T.Tm)
checkU c t = infer c t >>= \case
  (T.SU n, t') -> pure (n, t')
  (a, _) -> throwError $ "The term " ++ show t ++ " is expected to be a sized type, instead it has type " ++ show (reify (length c) a)

check :: MonadError String m => Ctxt -> S.Tm -> T.Sm -> m T.Tm
check c (S.Lam x t) (T.SPi a b) = do
  let sx = T.SVar $ length c
  t' <- check ((x, (a, sx)) : c) t (b sx)
  pure $ T.Lam t'
check _ (S.Lam {}) _ = throwError "Lambdas must have function types"
check c t a = do
  (b, t') <- infer c t
  let k = length c
  let a'  = reify k a
  let b'  = reify k b
  unless (a' == b') (throwError $ "When checking the term " ++ show t ++ "\nMismatch between\n" ++ show a' ++ "\n" ++ show b')
  pure t'

env :: Ctxt -> T.SEnv
env = map (snd . snd)

lkp :: MonadError String m => S.Nm -> Ctxt -> m (T.Sm, T.Tm)
lkp x = \case
  [] -> throwError $ "Undefined variable " ++ x
  (y, (a, _)) : ys
    | x == y -> pure (a, T.Var $ length ys)
    | otherwise -> lkp x ys