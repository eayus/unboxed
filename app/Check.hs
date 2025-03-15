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
    t' <- check c t T.SU
    let st = eval (env c) t'
    u' <- check ((x, (st, T.SVar (length c))) : c) u T.SU
    pure (T.SU, T.Pi t' u')
  S.U -> pure (T.SU, T.U)
  S.Ano t u -> do
    u' <- check c u T.SU
    let su = (eval (env c) u')
    t' <- check c t su
    pure (su, T.Ano t' u')
  S.Let x t u -> do
    (a, t') <- infer c t
    let st = eval (env c) t'
    (b, u') <- infer ((x, (a, st)) : c) u
    pure (b, T.Let t' u')

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
  unless (a' == b') (throwError $ "Mismatch between\n" ++ show a' ++ "\n" ++ show b')
  pure t'

env :: Ctxt -> T.Env
env = map (snd . snd)

lkp :: MonadError String m => S.Nm -> Ctxt -> m (T.Sm, T.Tm)
lkp x = \case
  [] -> throwError $ "Undefined variable " ++ x
  (y, (a, _)) : ys
    | x == y -> pure (a, T.Var $ length ys)
    | otherwise -> lkp x ys
