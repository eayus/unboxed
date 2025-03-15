module Main where

import Control.Monad.Except
import Control.Monad.IO.Class
import Parse
import System.Environment

type App = ExceptT String IO

app :: App ()
app = do
  fp <- liftIO getArgs >>= \case
    [x] -> pure x
    _ -> throwError "Program requires one argument, which is the file to process."
  tm <- parseFile fp
  liftIO $ print tm

main :: IO ()
main = runExceptT app >>= \case
  Left err -> putStrLn err
  Right () -> pure ()
