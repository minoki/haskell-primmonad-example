module MyLib (mkSieveST, mkSievePM, mkSievePMInlinable, mkSieveSTToPrim) where
import           Control.Monad (forM_, when)
import           Control.Monad.Primitive (PrimMonad (..), stToPrim)
import           Control.Monad.ST (ST)
import qualified Data.Vector.Unboxed.Mutable as VUM

mkSieveST :: Int -> ST s (VUM.MVector s Bool)
mkSieveST !n = do
  !vec <- VUM.replicate (n + 1) True
  VUM.write vec 0 False
  VUM.write vec 1 False
  forM_ [2..n] $ \i -> do
    !b <- VUM.read vec i
    when b $ do
      forM_ [i*i,i*(i+1)..n] $ \j ->
        VUM.write vec j False
  pure vec

{-# NOINLINE mkSievePM #-}
mkSievePM :: PrimMonad m => Int -> m (VUM.MVector (PrimState m) Bool)
mkSievePM !n = do
  !vec <- VUM.replicate (n + 1) True
  VUM.write vec 0 False
  VUM.write vec 1 False
  forM_ [2..n] $ \i -> do
    !b <- VUM.read vec i
    when b $ do
      forM_ [i*i,i*(i+1)..n] $ \j ->
        VUM.write vec j False
  pure vec

{-# INLINABLE mkSievePMInlinable #-}
mkSievePMInlinable :: PrimMonad m => Int -> m (VUM.MVector (PrimState m) Bool)
mkSievePMInlinable !n = do
  !vec <- VUM.replicate (n + 1) True
  VUM.write vec 0 False
  VUM.write vec 1 False
  forM_ [2..n] $ \i -> do
    !b <- VUM.read vec i
    when b $ do
      forM_ [i*i,i*(i+1)..n] $ \j ->
        VUM.write vec j False
  pure vec

{-# NOINLINE mkSieveSTToPrim #-}
mkSieveSTToPrim :: PrimMonad m => Int -> m (VUM.MVector (PrimState m) Bool)
mkSieveSTToPrim !n = stToPrim $ do
  !vec <- VUM.replicate (n + 1) True
  VUM.write vec 0 False
  VUM.write vec 1 False
  forM_ [2..n] $ \i -> do
    !b <- VUM.read vec i
    when b $ do
      forM_ [i*i,i*(i+1)..n] $ \j ->
        VUM.write vec j False
  pure vec
