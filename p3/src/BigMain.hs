module Main where

import Control.Monad ( void )
import Control.Monad.State
    ( MonadIO(..),
      MonadState(put, get),
      MonadTrans(lift),
      StateT(runStateT) )
import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )
import System.IO (hPutStrLn, stderr)

import While.Parser (parseWhile)
import While.Syntax (Stm)

import While.Big (evalS)
import While.Env (Env, emptyEnv)
import While.Monad (runWhile)

-- | Write a string to standard err, followed by a newline.
errStrLn :: MonadIO m => String -> InputT m ()
errStrLn msg = liftIO $ hPutStrLn stderr msg

runWhileStm :: Stm -> InputT (StateT Env IO) ()
runWhileStm stm = do env <- lift get
                     case runWhile (evalS stm) env of
                       Left msg         -> errStrLn msg
                       Right ((), env') -> lift $ put env'

driverLoop :: IO ()
driverLoop = void $ runStateT (runInputT defaultSettings loop) emptyEnv
   where
       loop :: InputT (StateT Env IO) ()
       loop = do
           minput <- getInputLine "> "
           case minput of
               Nothing       -> return ()
               Just ":quit"  -> return ()
               Just ":env"   -> do env <- lift get
                                   outputStrLn $ show env
                                   loop
               Just ":reset" -> do lift $ put emptyEnv
                                   loop
               Just input
                  | take 6 input == ":parse" -> do case parseWhile (drop 6 input) of
                                                     Nothing  -> errStrLn "Could not parse"
                                                     Just stm -> outputStrLn $ show stm
                                                   loop
               Just input    -> do case parseWhile input of
                                     Nothing  -> errStrLn "Could not parse"
                                     Just stm -> runWhileStm stm
                                   loop

main :: IO ()
main = driverLoop
