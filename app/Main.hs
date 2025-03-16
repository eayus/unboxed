module Main where

import Check
import Control.Monad.Except
import Control.Monad.IO.Class
import Norm
import Parse
import System.Environment

type App = ExceptT String IO

app :: App ()
app = do
  fp <- liftIO getArgs >>= \case
    [x] -> pure x
    _ -> throwError "Program requires exactly one argument, which is the file to process."
  t <- parseFile fp
  (a, t') <- infer [] t
  let a' = reify 0 a
  liftIO $ putStrLn $ show (reify 0 $ eval [] t') ++ " : " ++ show a'

main :: IO ()
main = runExceptT app >>= \case
  Left err -> putStrLn err
  Right () -> pure ()
