module Term where

data Tm
  = Var Int
  | Lam Tm
  | App Tm Tm
  | Pi Tm Tm
  | U Int
  | Ur
  | Nat
  | Num Int
  | Ano Tm Tm
  | Let Tm Tm
  | Box Tm
  | Bang Tm
  | New Gen
  | SigR Tm Tm
  | Array Tm Tm
  | Index Ptr
  deriving (Eq, Show)

data Gen
  = Pure Tm Int -- Val, Size
  | Replicate Tm Tm Int -- count, elem, sizeof(elem)
  | Pair Tm Gen
  | GAno Gen Tm
  deriving (Eq, Show)

data Ptr
  = Fst Ptr
  | Snd Ptr
  | Deref Tm
  | FstR Ptr
  | SndR Ptr
  | Elem Ptr Tm
  deriving (Eq, Show)

data Sm
  = SVar Int -- de Bruijn level
  | SLam (Sm -> Sm)
  | SApp Sm Sm
  | SPi Sm (Sm -> Sm)
  | SU Int
  | SUr
  | SNat
  | SNum Int
  | SAno Sm Sm
  | SLet Sm (Sm -> Sm)
  | SBox Sm
  | SBang Sm
  | SNew SGen
  | SSigR Sm (Sm -> Sm)
  | SArray Sm Sm
  | SIndex SPtr

data SGen
  = SPure Sm Int
  | SReplicate Sm Sm Int
  | SPair Sm SGen
  | SGAno SGen Sm

data SPtr
  = SFst SPtr
  | SSnd SPtr
  | SDeref Sm
  | SFstR SPtr
  | SSndR SPtr
  | SElem SPtr Sm

type SEnv = [Sm]