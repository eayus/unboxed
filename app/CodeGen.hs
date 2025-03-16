module CodeGen where

import LLVM.AST qualified as L hiding (function)
import LLVM.AST.IntegerPredicate qualified as L
import LLVM.AST.Type qualified as L
import LLVM.Context qualified as L
import LLVM.IRBuilder qualified as L
import Term


data Env = Env {
  vars :: [L.Operand],
  malloc :: L.Operand
}

type LLVM = L.ModuleBuilder

type CG = L.IRBuilderT LLVM

cg :: Env -> Tm -> CG L.Operand
cg r = \case
  Var l -> pure $ reverse (vars r) !! l
  Lam t -> undefined
  App t u -> undefined
  Pi {} -> pure it
  U {} -> pure it
  Ur -> pure it
  Nat -> pure it
  Num n -> pure $ int n
  Ano t _ -> cg r t
  Let t u -> cg r t >>= \t' -> cg (bind t' r) u
  Box {} -> pure it
  Bang {} -> pure it
  New g -> do
    i <- cgGenSize r g
    p <- L.call mallocTy (malloc r) [(i, [])]
    cgGen r p g
    pure p
  SigR {} -> pure it
  Array {} -> pure it
  Index p i -> do
    p' <- cgPtr r p
    L.load (bytes i) p' 1

-- Write the unsized data into the operand pointer.
cgGen :: Env -> L.Operand -> Gen -> CG ()
cgGen r p = \case
  Pure t _ -> do
    t' <- cg r t
    L.store t' 1 p -- The number is the alignment I think
  Replicate len t _ -> mdo
    -- For loop and fill the array
    t' <- cg r t
    len' <- cg r len
    ip <- L.alloca L.i64 (Just $ int 0) 1

    loop <- L.block
    i <- L.load L.i64 ip 1
    b <- L.icmp L.ULT i len'
    L.condBr b step done

    step <- L.block
    L.store t' 1 =<< offset p i
    i' <- L.add i (int 1)

    L.store i' 1 ip
    L.br loop

    done <- L.block
    pure ()
  Pair t g i -> do
    t' <- cg r t
    L.store t' 1 p
    p' <- offset p (int i)
    cgGen r p' g
  GAno g _ -> cgGen r p g

cgPtr :: Env -> Ptr -> CG L.Operand
cgPtr r = \case
  Fst t -> undefined
  Snd t -> undefined
  Deref t -> cg r t
  FstR t -> cgPtr r t
  SndR t i -> do
    t' <- cgPtr r t
    offset t' (int i)
  Elem p t -> do
    t' <- cg r t
    undefined

cgGenSize :: Env -> Gen -> CG L.Operand
cgGenSize r = \case
  Pure _ n -> pure $ int n
  Replicate n _ i -> do
    n' <- cg r n
    L.mul n' (int i)
  Pair _ g i -> do
    j <- cgGenSize r g
    L.add (int i) j
  GAno g _ -> cgGenSize r g

bytes :: Int -> L.Type
bytes i = L.StructureType True (replicate i L.i8)

-- Offset a pointer by n bites
offset :: L.Operand -> L.Operand -> CG L.Operand
offset p i = undefined

int :: Int -> L.Operand
int = L.int64 . toInteger

unit :: L.Type
unit = L.StructureType True [] -- True means it's packed

it :: L.Operand
it = L.struct Nothing True []

bind :: L.Operand -> Env -> Env
bind t e = e { vars = t : vars e }

mallocTy :: L.Type
mallocTy = L.FunctionType L.ptr [L.i64] False