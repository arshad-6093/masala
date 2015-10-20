{-# LANGUAGE ScopedTypeVariables #-}

module Masala.VM.Memory where

import Masala.VM.Types
import Masala.Word
import Data.Maybe
import qualified Data.Map.Strict as M
import Control.Lens

mload :: Monad m => U256 -> VM m U256
mload i | i +^ 31 <= i = return 0
        | otherwise = do
  m <- use mem
  return $ head . u8sToU256s . map (fromMaybe 0 . (`M.lookup` m)) $ [i .. i+31]


mstore :: Monad m => U256 -> U256 -> VM m ()
mstore i v = mem %= M.union (M.fromList $ reverse $ zip (reverse [i .. i + 31]) (reverse (u256ToU8s v) ++ repeat 0))

mstore8 :: Monad m => U256 -> U8 -> VM m ()
mstore8 i b = mem %= M.insert i b


mloads :: Monad m => U256 -> U256 -> VM m [U8]
mloads loc len | len < 1 = return []
               | otherwise = do
  m <- use mem
  return $ map (fromMaybe 0 . (`M.lookup` m)) $ [loc .. loc + len - 1]


mstores :: Monad m => U256 -> U256 -> U256 -> [U8] -> VM m ()
mstores memloc off len v
    | len < 1 = return ()
    | off +^ len > fromIntegral (length v) = return ()
    | otherwise = mem %= M.union (M.fromList $ zip [memloc .. memloc + len - 1] (drop (fromIntegral off) v))



infixl 6 +^

(+^) :: (Integral a, Bounded a) => a -> a -> a
(+^) = overflowOp (+)

overflowOp :: forall a . (Integral a, Bounded a) => (Integer -> Integer -> Integer) -> a -> a -> a
overflowOp f a b = if r > fromIntegral (maxBound :: a) then maxBound else fromIntegral r
    where r = f (fromIntegral a) (fromIntegral b)


msize :: Monad m => VM m U256
msize = (* 32) . ceiling .  (/ (32 :: Float)) . fromIntegral . succ . maximum' . M.keys <$> use mem
    where maximum' [] = 0
          maximum' vs = maximum vs




sload :: MonadExt m => U256 -> VM m U256
sload i = do
  s <- view address
  fromMaybe 0 <$> extLoad s i

sstore :: MonadExt m => U256 -> U256 -> VM m ()
sstore a b = do
  s <- view address
  extStore s a b
