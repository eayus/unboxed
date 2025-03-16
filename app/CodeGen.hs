module CodeGen where

import LLVM.AST qualified as L hiding (function)
import LLVM.AST.Type qualified as L
import LLVM.Context qualified as L
import LLVM.IRBuilder qualified as L
import Term


type Env = [L.Operand]

type LLVM = L.ModuleBuilder

type CG = L.IRBuilderT LLVM

cg :: Env -> Tm -> CG L.Operand
cg r = \case
  Var l -> pure $ reverse r !! l
  Lam t -> undefined
  App t u -> undefined
  Pi {} -> pure it
  U {} -> pure it
  Ur -> pure it
  Nat -> pure it
  Num n -> pure $ int n
  Ano t _ -> cg r t
  Let t u -> cg r t >>= \t' -> cg (t' : r) u
  Box _ -> pure it
  Bang _ -> pure it
  New g -> do
    i <- cgGenSize r g
    undefined

cgGenSize :: Env -> Gen -> CG L.Operand
cgGenSize r = \case
  Pure _ n -> pure $ int n
  Replicate n _ i -> do
    n' <- cg r n
    L.mul n' (int i)

int :: Int -> L.Operand
int = L.int64 . toInteger

unit :: L.Type
unit = L.StructureType False []

it :: L.Operand
it = L.struct Nothing False []