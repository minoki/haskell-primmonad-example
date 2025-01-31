module Main where
import qualified Brainfuck
import           Control.Exception (catch)
import           Data.Char (chr, ord)
import           Data.Word (Word8)
import           System.Environment (getArgs)
import           System.IO (hPutStrLn, stderr)

put :: Word8 -> IO ()
put c = putChar (chr $ fromIntegral c)

get :: IO Word8
get = do
  c <- getChar `catch` \(_ :: IOError) -> pure '\xFF'
  pure $ fromIntegral $ ord c

main :: IO ()
main = do
  args <- getArgs
  case args of
    "io":programName:_ -> do
      program <- Brainfuck.parseAll <$> readFile programName
      state <- Brainfuck.newState 67108864
      _state <- Brainfuck.runIO program state
      pure ()
    "pm":programName:_ -> do
      program <- Brainfuck.parseAll <$> readFile programName
      state <- Brainfuck.newState 67108864
      _state <- Brainfuck.runPM put get program state
      pure ()
    "pmi":programName:_ -> do
      program <- Brainfuck.parseAll <$> readFile programName
      state <- Brainfuck.newState 67108864
      _state <- Brainfuck.runPMInlinable put get program state
      pure ()
    "mixed":programName:_ -> do
      program <- Brainfuck.parseAll <$> readFile programName
      state <- Brainfuck.newState 67108864
      _state <- Brainfuck.runPMMixed put get program state
      pure ()
    _ -> hPutStrLn stderr "usage: simple-bf io/pm/pmi/mixed <program.bf>"
