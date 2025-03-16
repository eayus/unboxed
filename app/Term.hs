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
  | Bang Tm Int -- type, sizeof(type)
  | New Gen
  | SigR Tm Tm Int -- Fst, Snd, sizeof(Fst)
  | Array Tm Tm Int -- len, el, sizeof(el)
  | Index Ptr Int -- ptr, sizeof(*ptr)
  deriving (Eq, Show)

data Gen
  = Pure Tm Int -- Val, Size
  | Replicate Tm Tm Int -- count, elem, sizeof(elem)
  | Pair Tm Gen Int -- fst, snd, sizeof(fst)
  | GAno Gen Tm
  deriving (Eq, Show)

data Ptr
  = Fst Ptr
  | Snd Ptr
  | Deref Tm
  | FstR Ptr
  | SndR Ptr Int -- pair, sizeof(fst(pair))
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
  | SBang Sm Int
  | SNew SGen
  | SSigR Sm (Sm -> Sm) Int
  | SArray Sm Sm Int
  | SIndex SPtr Int

data SGen
  = SPure Sm Int
  | SReplicate Sm Sm Int
  | SPair Sm SGen Int
  | SGAno SGen Sm

data SPtr
  = SFst SPtr
  | SSnd SPtr
  | SDeref Sm
  | SFstR SPtr
  | SSndR SPtr Int
  | SElem SPtr Sm

type SEnv = [Sm]