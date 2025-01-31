module Main where
import           Control.Monad.ST (stToIO)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Writer
import qualified Data.Vector.Unboxed.Mutable as VUM
import           MyLib
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

type M = ReaderT Int (StateT Int (WriterT String IO))

runM :: M a -> IO a
runM action = fst <$> runWriterT (evalStateT (runReaderT action 0) 0)

main :: IO ()
main = do
  args <- getArgs
  let n = 10000000
  vec <- case args of
    "st":_ -> stToIO $ mkSieveST n
    "pm":_ -> mkSievePM n
    "pmi":_ -> mkSievePMInlinable n
    "sttoprim":_ -> mkSieveSTToPrim n
    "stacked-pmi":_ -> runM (mkSievePMInlinable n)
    "stacked-sttoprim":_ -> runM (mkSieveSTToPrim n)
    _ -> do hPutStrLn stderr "argument: st,pm,pmi"
            exitFailure
  m <- VUM.foldl (\ !acc !b -> if b then acc + 1 else acc :: Int) 0 vec
  print m
