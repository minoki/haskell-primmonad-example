module Main where
import qualified Brainfuck
import           Data.Char (chr, ord)
import           Data.Word (Word8)
import           System.Environment (getArgs)
import           System.IO (hPutStrLn, stderr)
import           Control.Monad.Cont
import           Control.Monad.Reader
import           Control.Monad.ST
import           Data.STRef
import           System.Exit (exitFailure)

data Answer s = NeedMoreInput String (String -> ST s (Answer s))
              | Done String

type K s = ContT (Answer s) (ST s)
type M s = ReaderT (STRef s String, STRef s String, String -> K s String) (K s)

put :: Word8 -> M s ()
put c = do
  let c' = chr $ fromIntegral c
  (_, bufOut, _) <- ask
  lift $ lift $ modifySTRef' bufOut (c' :)

get :: M s Word8
get = do
  (bufIn, bufOut, needMoreInput) <- ask
  input <- lift $ lift $ readSTRef bufIn
  case input of
    c:cs -> do
      lift $ lift $ writeSTRef bufIn cs
      pure $ fromIntegral $ ord c
    [] -> do
      output <- fmap reverse $ lift $ lift $ readSTRef bufOut
      lift $ lift $ writeSTRef bufOut []
      newInput <- lift $ needMoreInput output
      case newInput of
        c:cs -> do
          lift $ lift $ writeSTRef bufIn cs
          pure $ fromIntegral $ ord c
        [] -> pure 0xFF

interpret :: M s (Brainfuck.State s) -> ST s (Answer s)
interpret run = do
  bufIn <- newSTRef []
  bufOut <- newSTRef []
  let action = callCC $ \k -> do
        let needMoreInput output = callCC $ \l -> k (NeedMoreInput output (\newInput -> evalContT (l newInput)))
        _finalState <- runReaderT run (bufIn, bufOut, needMoreInput)
        output <- fmap reverse $ lift $ readSTRef bufOut
        pure $ Done output
  evalContT action

main :: IO ()
main = do
  args <- getArgs
  ans0 <- case args of
    "pm":programName:_ -> do
      program <- Brainfuck.parseAll <$> readFile programName
      state <- Brainfuck.newState 67108864
      stToIO $ interpret $ Brainfuck.runPM put get program state
    "pmi":programName:_ -> do
      program <- Brainfuck.parseAll <$> readFile programName
      state <- Brainfuck.newState 67108864
      stToIO $ interpret $ Brainfuck.runPMInlinable put get program state
    "mixed":programName:_ -> do
      program <- Brainfuck.parseAll <$> readFile programName
      state <- Brainfuck.newState 67108864
      stToIO $ interpret $ Brainfuck.runPMMixed put get program state
    _ -> do hPutStrLn stderr "usage: cps-bf pm/pmi/mixed <program.bf>"
            exitFailure
  case ans0 of
    Done s -> putStrLn $ "output[0]: " ++ s
    NeedMoreInput s more -> do
      putStrLn $ "output[0]: " ++ s
      ans1 <- stToIO $ more "Hello!"
      case ans1 of
        Done s1 -> putStrLn $ "output[1]: " ++ s1
        NeedMoreInput s1 _ -> do
          putStrLn $ "output[1]: " ++ s1
