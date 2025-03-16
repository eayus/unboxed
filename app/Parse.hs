module Parse (parseFile) where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Data.Functor
import Data.Void
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type P = Parsec Void String

parseFile :: (MonadError String m, MonadIO m) => FilePath -> m Tm
parseFile fp = do
  s <- liftIO $ readFile fp
  case runParser (sc *> pTm <* eof) fp s of
    Left e -> throwError $ errorBundlePretty e
    Right x -> pure x

pTm :: P Tm
pTm = foldl1 App <$> some pAtom

pAtom :: P Tm
pAtom = choice $ map try [pIndex, pU, pUr, pBang, pArray, pBox, pNew, pLet, pVar, pLam, pSigR, pPi, pAno, pTm', pNat, pNum]
  where
    pVar = Var <$> pNm
    pLam = do sym "\\"; x <- pNm; sym "."; Lam x <$> pTm
    pPi = do lp; x <- pNm; sym ":"; t <- pTm; rp; sym "->"; Pi x t <$> pTm
    pU = do sym "U"; U <$> pInt
    pUr = sym "Ur" $> Ur
    pNat = sym "Nat" $> Nat
    pNum = Num <$> pInt
    pAno = do lp; t <- pTm; sym ":"; u <- pTm; rp; pure $ Ano t u
    pLet = do sym "let"; x <- pNm; sym "="; t <- pTm; sym ";"; Let x t <$> pTm
    pBox = do sym "Box"; Box <$> pTm
    pNew = do sym "new"; New <$> pGen
    pBang = do sym "!"; Bang <$> pTm
    pSigR = do lp; x <- pNm; sym ":"; t <- pTm; rp; sym "&r"; u <- pAtom; pure $ SigR x t u
    pArray = do sym "Array"; t <- pAtom; u <- pAtom; pure $ Array t u
    pIndex = do sym "index"; Index <$> pPtr
    pTm' = do lp; t <- pTm; rp; pure t-- lp *> pTm <* rp

pGen :: P Gen
pGen = choice $ map try [pGAno, pPure, pReplicate, pPair, pGen']
  where
    pPure = do sym "pure"; lp; t <- pTm; sym ":"; a <- pTm; rp; pure $ Pure t a
    pReplicate = do sym "replicate"; n <- pAtom; lp; t <- pTm; sym ":"; a <- pTm; rp; pure $ Replicate n t a
    pPair = do lp; t <- pTm; sym ","; u <- pGen; rp; pure $ Pair t u
    pGAno = do lp; t <- pGen; sym ":"; u <- pTm; rp; pure $ GAno t u
    pGen' = do lp; t <- pGen; rp; pure t-- lp *> pGen <* rp

pPtr :: P Ptr
pPtr = choice $ map try [pFst, pSnd, pDeref, pFstR, pSndR, pElem, pPtr']
  where
    pFst = do sym "fst"; Fst <$> pPtr
    pSnd = do sym "snd"; Snd <$> pPtr
    pDeref = do sym "@"; Deref <$> pAtom
    pFstR = do sym "fstr"; FstR <$> pPtr
    pSndR = do sym "sndr"; SndR <$> pPtr
    pElem = do sym "elem"; p <- pPtr; t <- pAtom; pure $ Elem p t
    pPtr' = do lp; p <- pPtr; rp; pure p

pNm :: P Nm
pNm = lexeme do
  c <- letterChar
  cs <- many (alphaNumChar <|> char '-')
  let s = c : cs
  guard (s `notElem` reserved)
  pure s

pInt :: P Int
pInt = lexeme L.decimal

-- Symbols

lp :: P ()
lp = sym "("

rp :: P ()
rp = sym ")"

-- Lexing

reserved :: [String]
reserved = ["let", "U", "Ur", "Box", "Array", "Nat", "new", "pure", "replicate", "fst", "fstr", "snd", "sndr", "index"]

lexeme :: P a -> P a
lexeme = L.lexeme sc

sym :: String -> P ()
sym = void . L.symbol sc

sc :: P ()
sc =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "{-" "-}")