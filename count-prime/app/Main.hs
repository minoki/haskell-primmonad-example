module Main where
import           Control.Monad.ST (stToIO)
import qualified Data.Vector.Unboxed.Mutable as VUM
import           MyLib
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  let n = 10000000
  vec <- case args of
    "st":_ -> stToIO $ mkSieveST n
    "pm":_ -> mkSievePM n
    "pmi":_ -> mkSievePMInlinable n
    _ -> do hPutStrLn stderr "argument: st,pm,pmi"
            exitFailure
  m <- VUM.foldl (\ !acc !b -> if b then acc + 1 else acc :: Int) 0 vec
  print m
