{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Brainfuck
  ( Instruction (..)
  , parse
  , parseAll
  , State (..)
  , newState
  , runIO
  , runPM
  , runPMInlinable
  , runPMMixed
  ) where
import           Control.Exception (catch)
import           Control.Monad.Primitive (PrimMonad (..), stToPrim)
import           Control.Monad.ST (RealWorld, ST)
import           Data.Char (chr, ord)
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word (Word8)

data Instruction = Arith !Word8
                 | Pointer !Int
                 | Input
                 | Output
                 | BeginLoop [Instruction]
                 | EndLoop [Instruction]
                 deriving (Eq, Show)

takePlusMinus :: Int -> String -> (Int, String)
takePlusMinus !acc []         = (acc, [])
takePlusMinus !acc ('+':xs)   = takePlusMinus (acc + 1) xs
takePlusMinus !acc ('-':xs)   = takePlusMinus (acc - 1) xs
takePlusMinus !acc xs@('>':_) = (acc, xs)
takePlusMinus !acc xs@('<':_) = (acc, xs)
takePlusMinus !acc xs@(',':_) = (acc, xs)
takePlusMinus !acc xs@('.':_) = (acc, xs)
takePlusMinus !acc xs@('[':_) = (acc, xs)
takePlusMinus !acc xs@(']':_) = (acc, xs)
takePlusMinus !acc (_:xs)     = takePlusMinus acc xs

takeLeftRight :: Int -> String -> (Int, String)
takeLeftRight !acc []         = (acc, [])
takeLeftRight !acc ('>':xs)   = takeLeftRight (acc + 1) xs
takeLeftRight !acc ('<':xs)   = takeLeftRight (acc - 1) xs
takeLeftRight !acc xs@('+':_) = (acc, xs)
takeLeftRight !acc xs@('-':_) = (acc, xs)
takeLeftRight !acc xs@(',':_) = (acc, xs)
takeLeftRight !acc xs@('.':_) = (acc, xs)
takeLeftRight !acc xs@('[':_) = (acc, xs)
takeLeftRight !acc xs@(']':_) = (acc, xs)
takeLeftRight !acc (_:xs)     = takeLeftRight acc xs

parse :: [[Instruction]] -> String -> [Instruction]
parse [] [] = []
parse (_:_) [] = error "Unmatched bracket"
parse loops ('+':xs) = let (delta, xss) = takePlusMinus 1 xs
                       in Arith (fromIntegral delta) : parse loops xss
parse loops ('-':xs) = let (delta, xss) = takePlusMinus (-1) xs
                       in Arith (fromIntegral delta) : parse loops xss
parse loops ('>':xs) = let (delta, xss) = takeLeftRight 1 xs
                       in Pointer (fromIntegral delta) : parse loops xss
parse loops ('<':xs) = let (delta, xss) = takeLeftRight (-1) xs
                       in Pointer (fromIntegral delta) : parse loops xss
parse loops (',':xs) = Input : parse loops xs
parse loops ('.':xs) = Output : parse loops xs
parse loops ('[':xs) = let loop = parse (loop:loops) xs
                           find :: Int -> [Instruction] -> [Instruction]
                           find !_ [] = error "Unmatched bracket"
                           find !level (BeginLoop _ : insns) = find (level + 1) insns
                           find 0 (EndLoop _ : insns) = insns
                           find level (EndLoop _ : insns) = find (level - 1) insns
                           find level (_ : insns) = find level insns
                           !endOfLoop = find 0 loop
                       in BeginLoop endOfLoop : loop
parse (loop:loops) (']':xs) = EndLoop loop : parse loops xs
parse [] (']':_) = error "Unmatched bracket"
parse loops (_:xs) = parse loops xs

parseAll :: String -> [Instruction]
parseAll = parse []

data State s = State { pointer :: !Int
                     , array   :: !(VUM.MVector s Word8)
                     }

newState :: PrimMonad m => Int -> m (State (PrimState m))
newState !size = do
  a <- VUM.replicate size 0
  pure $ State { pointer = 0, array = a }

runIO :: [Instruction] -> State RealWorld -> IO (State RealWorld)
runIO insns State { pointer = initialPointer, array = array } = do
  let loop !pointer [] = pure pointer
      loop !pointer (insn : insns) = do
        case insn of
          Arith delta -> VUM.modify array (+ delta) pointer >> loop pointer insns
          Pointer delta -> loop (pointer + delta) insns
          Output -> do
            !c <- VUM.read array pointer
            putChar (chr $ fromIntegral c)
            loop pointer insns
          Input -> do
            !c <- getChar `catch` \(_ :: IOError) -> pure '\xFF'
            VUM.write array pointer (fromIntegral $ ord c)
            loop pointer insns
          BeginLoop alt -> do
            !v <- VUM.read array pointer
            if v == 0
              then loop pointer alt
              else loop pointer insns
          EndLoop alt -> do
            !v <- VUM.read array pointer
            if v == 0
              then loop pointer insns
              else loop pointer alt
  !pointer <- loop initialPointer insns
  pure $ State { pointer = pointer, array = array }

{-# NOINLINE runPM #-}
runPM :: PrimMonad m => (Word8 -> m ()) -> m Word8 -> [Instruction] -> State (PrimState m) -> m (State (PrimState m))
runPM put get insns State { pointer = initialPointer, array = array } = do
  let loop !pointer [] = pure pointer
      loop !pointer (insn : insns) = do
        case insn of
          Arith delta -> VUM.modify array (+ delta) pointer >> loop pointer insns
          Pointer delta -> loop (pointer + delta) insns
          Output -> do
            !c <- VUM.read array pointer
            put c
            loop pointer insns
          Input -> do
            !c <- get
            VUM.write array pointer c
            loop pointer insns
          BeginLoop alt -> do
            !v <- VUM.read array pointer
            if v == 0
              then loop pointer alt
              else loop pointer insns
          EndLoop alt -> do
            !v <- VUM.read array pointer
            if v == 0
              then loop pointer insns
              else loop pointer alt
  !pointer <- loop initialPointer insns
  pure $ State { pointer = pointer, array = array }

{-# INLINABLE runPMInlinable #-}
runPMInlinable :: PrimMonad m => (Word8 -> m ()) -> m Word8 -> [Instruction] -> State (PrimState m) -> m (State (PrimState m))
runPMInlinable put get insns State { pointer = initialPointer, array = array } = do
  let loop !pointer [] = pure pointer
      loop !pointer (insn : insns) = do
        case insn of
          Arith delta -> VUM.modify array (+ delta) pointer >> loop pointer insns
          Pointer delta -> loop (pointer + delta) insns
          Output -> do
            !c <- VUM.read array pointer
            put c
            loop pointer insns
          Input -> do
            !c <- get
            VUM.write array pointer c
            loop pointer insns
          BeginLoop alt -> do
            !v <- VUM.read array pointer
            if v == 0
              then loop pointer alt
              else loop pointer insns
          EndLoop alt -> do
            !v <- VUM.read array pointer
            if v == 0
              then loop pointer insns
              else loop pointer alt
  !pointer <- loop initialPointer insns
  pure $ State { pointer = pointer, array = array }

runPMMixed :: forall m. PrimMonad m => (Word8 -> m ()) -> m Word8 -> [Instruction] -> State (PrimState m) -> m (State (PrimState m))
runPMMixed put get insns State { pointer = initialPointer, array = array } = do
  let loopST :: Int -> [Instruction] -> ST (PrimState m) ([Instruction], Int)
      loopST !pointer [] = pure ([], pointer)
      loopST !pointer insns0@(insn : insns) = do
        case insn of
          Arith delta -> VUM.modify array (+ delta) pointer >> loopST pointer insns
          Pointer delta -> loopST (pointer + delta) insns
          Output -> pure (insns0, pointer)
          Input -> pure (insns0, pointer)
          BeginLoop alt -> do
            !v <- VUM.read array pointer
            if v == 0
              then loopST pointer alt
              else loopST pointer insns
          EndLoop alt -> do
            !v <- VUM.read array pointer
            if v == 0
              then loopST pointer insns
              else loopST pointer alt
  let loop !pointer [] = pure pointer
      loop !pointer (Output : insns) = do
        !c <- VUM.read array pointer
        put c
        continue pointer insns
      loop !pointer (Input : insns) = do
        !c <- get
        VUM.write array pointer c
        continue pointer insns
      loop !pointer insns = continue pointer insns
      continue pointer insns = do
        (insns', pointer') <- stToPrim $ loopST pointer insns
        loop pointer' insns'
  !pointer <- loop initialPointer insns
  pure $ State { pointer = pointer, array = array }
