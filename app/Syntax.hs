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
  | Index Ptr
  deriving Show

-- The form of expressions which generate unsized data.
data Gen
  = Pure Tm Tm -- Val, Ty
  | Replicate Tm Tm Tm -- count, elem, typeof(elem)
  | Pair Tm Gen
  | GAno Gen Tm
  deriving Show

-- The form of expressions which access (index into) unsized data.
data Ptr
  = Fst Ptr
  | Snd Ptr
  | Deref Tm
  | FstR Ptr
  | SndR Ptr
  | Elem Ptr Tm
  deriving Show