{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Masala.VM where

import Data.DoubleWord
import Data.Bits
import Data.Word
import GHC.Generics
import Control.Monad.Reader
import Control.Monad.State.Strict hiding (set)
import Control.Lens
import Masala.Instruction
import Masala.Gas
import Control.Monad.Error
import qualified Data.Vector as V
import Control.Applicative
import Prelude hiding (LT,GT,EQ,log)
import Numeric
import Data.Char (intToDigit)


type Stack = [U256]
type Prog = V.Vector ByteCode
type Ctr = Int
data VMState = VMState {
      _stack :: Stack
    , _ctr :: Ctr
    , _gas :: Gas
} deriving (Eq,Show)

data Env = Env {
      debug :: Bool
    , prog :: Prog
    , address :: U256
    , origin, caller :: U256
    , balance, envGas, gasPrice, callValue :: U256
    , prevHash, coinbase, timestamp :: U256
    , number, difficulty, gaslimit :: U256
}

type VM m = (Functor m, Monad m, Applicative m,
             MonadIO m,
             MonadState VMState m,
             MonadError String m,
             MonadReader Env m)

data ControlFlow =
          Next
        | Stop
        | Jump Int
    deriving (Show)

stack :: Lens' VMState Stack
stack f s = fmap (\a -> s { _stack = a }) (f $ _stack s)
ctr :: Lens' VMState Ctr
ctr f s = fmap (\a -> s { _ctr = a }) (f $ _ctr s)
gas :: Lens' VMState Gas
gas f s = fmap (\a -> s { _gas = a }) (f $ _gas s)

push :: (VM m) => U256 -> m ()
push i = stack %= (i:)


pops :: Int -> VM m => m [U256]
pops n = do
  s <- use stack
  if n > length s
  then err $ "Stack underflow, expected " ++ show n
  else do
    stack .= drop n s
    return (take n s)

current :: VM m => m ByteCode
current = (V.!) <$> reader prog <*> use ctr

err :: VM m => String -> m a
err msg = do
  idx <- use ctr
  bc <- current
  throwError $ msg ++ " (index " ++ show idx ++
          ", value " ++ show bc ++ ")"

forward :: VM m => m Bool
forward = do
  c <- use ctr
  p <- reader prog
  if c + 1 >= V.length p
  then return False
  else do
    ctr .= c + 1
    return True


runVM :: Env -> IO (Either String ())
runVM env = evalStateT (runReaderT (runErrorT go) env)
                    (VMState [] 0 0)
    where go = do
            cf <- exec
            case cf of
              Next -> do
                      notDone <- forward
                      if notDone
                      then go
                      else do
                        d <- reader debug
                        vm <- get
                        when d $ liftIO $ print vm
                        return ()

run_ :: String -> IO (Either String ())
run_ = runBC_ . parseHex

runBC_ :: [ByteCode] -> IO (Either String ())
runBC_ bc = runVM (Env True (V.fromList bc) 0 0 0 0 0 0 0 0 0 0 0 0 0)

bin_ i = showIntAtBase 2 intToDigit i ""

exec :: VM m => m ControlFlow
exec = do
  bc <- current
  case bc of
    PushV _ -> err "Push value at top-level"
    Inst i -> do
            let (Spec _ stackin _ pspec) = spec i
            svals <- pops stackin
            d <- reader debug
            when d $ debugOut i svals
            dispatch i (pspec,svals)

debugOut :: VM m => Instruction -> [U256] -> m ()
debugOut i svals = do
  vm <- get
  liftIO $ print (i,svals,vm)

next :: VM m => m ControlFlow
next = return Next

pushb :: VM m => Bool -> m ()
pushb b = push $ if b then 1 else 0

sgn :: U256 -> S256
sgn = fromIntegral

pushs :: VM m => S256 -> m ()
pushs = push . fromIntegral



dup :: VM m => Int -> m ()
dup n = stackAt (n - 1) >>= push

stackAt :: VM m => Int -> m U256
stackAt n = do
  s <- firstOf (ix n) <$> use stack
  case s of
    Nothing -> err $ "stackAt " ++ show n ++ ": stack underflow"
    Just w -> return w

swap :: VM m => Int -> m ()
swap n = do
  s0 <- stackAt 0
  sn <- stackAt n
  stack %= set (ix 0) sn . set (ix n) s0

log :: VM m => Int -> m ()
log = undefined

sdiv :: S256 -> S256 -> S256
sdiv a b | b == 0 = 0
         | a == bigneg || b == (-1) = bigneg
         | otherwise = a `div` b
         where bigneg = (-2) ^ (255 :: Int)

-- TODO: C++ code (per tests) routinely masks after (t - 3) bits whereas this
-- code seems to do the right thing per spec.
signextend :: Int -> U256 -> U256
signextend k v 
    | k > 31 = v
    | otherwise = 
        let t = (k * 8) + 7
            mask = ((1 :: U256) `shiftL` t) - 1
        in if v `testBit` t
           then v .|. complement mask
           else v .&. mask

byte :: Int -> U256 -> U256
byte p v 
    | p > 31 = 0
    | otherwise = (v `shiftR` (8 * (31 - p))) .&. 0xff


dispatch :: VM m => Instruction -> (ParamSpec,[U256]) -> m ControlFlow
dispatch STOP _ = return Stop
dispatch _ (Push _,_) = do
  notDone <- forward
  if notDone
  then do
    n <- current
    case n of
      (PushV w) -> push w >> next
      _ -> err "Push: Instruction encountered"
  else err "Push: at EOF"
dispatch ADD (_,[a,b]) = push (a + b) >> next
dispatch MUL (_,[a,b]) = push (a * b) >> next
dispatch SUB (_,[a,b]) = push (a - b) >> next
dispatch DIV (_,[a,b]) =
    push (if b == 0 then 0 else a `div` b) >> next
dispatch SDIV (_,[a,b]) = pushs (sgn a `sdiv` sgn b) >> next
dispatch MOD (_,[a,b]) = push (a `mod` b) >> next
dispatch SMOD (_,[a,b]) = pushs (sgn a `mod` sgn b) >> next
dispatch ADDMOD (_,[a,b,c]) = push (a + b `mod` c) >> next
dispatch MULMOD (_,[a,b,c]) = push (a * b `mod` c) >> next
dispatch EXP (_,[a,b]) = push (a ^ b) >> next
dispatch SIGNEXTEND (_,[a,b]) = push (fromIntegral a `signextend` b) >> next
dispatch LT (_,[a,b]) = pushb (a < b) >> next
dispatch GT (_,[a,b]) = pushb (a > b) >> next
dispatch SLT (_,[a,b]) = pushb (sgn a < sgn b) >> next
dispatch SGT (_,[a,b]) = pushb (sgn a > sgn b) >> next
dispatch EQ (_,[a,b]) = pushb (a == b) >> next
dispatch ISZERO (_,[a]) = pushb (a == 0) >> next
dispatch AND (_,[a,b]) = push (a .&. b) >> next
dispatch OR (_,[a,b]) = push (a .|. b) >> next
dispatch XOR (_,[a,b]) = push (a `xor` b) >> next
dispatch NOT (_,[a]) = push (complement a) >> next
dispatch BYTE (_,[a,b]) = push (fromIntegral a `byte` b) >> next
dispatch SHA3 _ = err "TODO"
dispatch ADDRESS _ = reader address >>= push >> next
dispatch BALANCE _ = err "TODO"
dispatch ORIGIN _ = reader origin >>= push >> next
dispatch CALLER _ = reader caller >>= push >> next
dispatch CALLVALUE _ = reader callValue >>= push >> next
dispatch CALLDATALOAD _ = err "TODO"
dispatch CALLDATASIZE _ = err "TODO"
dispatch CALLDATACOPY _ = err "TODO"
dispatch CODESIZE _ = err "TODO"
dispatch CODECOPY _ = err "TODO"
dispatch GASPRICE _ = reader gasPrice >>= push >> next
dispatch EXTCODESIZE _ = err "TODO"
dispatch EXTCODECOPY _ = err "TODO"
dispatch BLOCKHASH _ = err "TODO"
dispatch COINBASE _ = reader coinbase >>= push >> next
dispatch TIMESTAMP _ = reader timestamp >>= push >> next
dispatch NUMBER _ = reader number >>= push >> next
dispatch DIFFICULTY _ = reader difficulty >>= push >> next
dispatch GASLIMIT _ = reader gaslimit >>= push >> next
dispatch POP _ = pops 1 >> next
dispatch MLOAD _ = err "TODO"
dispatch MSTORE _ = err "TODO"
dispatch MSTORE8 _ = err "TODO"
dispatch SLOAD _ = err "TODO"
dispatch SSTORE _ = err "TODO"
dispatch JUMP _ = err "TODO"
dispatch JUMPI _ = err "TODO"
dispatch PC _ = fromIntegral <$> use ctr >>= push >> next
dispatch MSIZE _ = err "TODO"
dispatch GAS _ = err "TODO"
dispatch JUMPDEST _ = err "TODO"
dispatch _ (Dup n,_) = dup n >> next
dispatch _ (Swap n,_) = swap n >> next
dispatch _ (Log n,_) = log n >> next
dispatch CREATE _ = err "TODO"
dispatch CALL _ = err "TODO"
dispatch CALLCODE _ = err "TODO"
dispatch RETURN _ = err "TODO"
dispatch SUICIDE _ = err "TODO"
dispatch _ ps = err $ "Unsupported operation [" ++ show ps ++ "]"
