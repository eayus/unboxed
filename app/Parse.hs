module Parse (parseFile) where

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
  where
    pAtom = choice [pVar, pLam, pPi, pU, pAno, pLet, pTm']
    pVar = Var <$> pNm
    pLam = do sym "\\"; x <- pNm; sym "."; Lam x <$> pTm
    pPi = try do lp; x <- pNm; sym ":"; t <- pTm; rp; sym "->"; Pi x t <$> pTm
    pU = sym "U" $> U
    pAno = do lp; t <- pTm; sym ":"; u <- pTm; rp; pure $ Ano t u
    pLet = do sym "let"; x <- pNm; sym "="; t <- pTm; sym ";"; Let x t <$> pTm
    pTm' = lp *> pTm <* rp

pNm :: P Nm
pNm = lexeme do
  c <- letterChar
  cs <- many alphaNumChar
  pure $ c : cs

-- Symbols

lp :: P ()
lp = sym "("

rp :: P ()
rp = sym ")"

-- Lexing

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