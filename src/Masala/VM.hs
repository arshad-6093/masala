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
import Control.Monad.State.Strict
import Control.Lens
import Masala.Instruction
import Control.Monad.Error
import qualified Data.Vector as V
import Control.Applicative
import Prelude hiding (LT,GT,EQ)

type Stack = [U256]
type Prog = V.Vector ByteCode

data VMState = VMState {
      _stack :: Stack
    , _prog :: Prog
    , _ctr :: Int
}

type VM m = (MonadState VMState m, Monad m, MonadError String m)

stack :: Lens' VMState Stack
stack f s = fmap (\a -> s { _stack = a }) (f $ _stack s)  
prog :: Lens' VMState Prog
prog f s = fmap (\a -> s { _prog = a }) (f $ _prog s)
ctr :: Lens' VMState Int
ctr f s = fmap (\a -> s { _ctr = a }) (f $ _ctr s)

push :: (VM m) => U256 -> m ()
push i = stack %= (i:)

pop :: (VM m) => m U256
pop = do
  s <- use stack
  case s of 
    [] -> throwError "empty stack"
    (v:vs) -> do
           stack .= vs
           return v

pops :: Int -> VM m => m [U256]
pops n = do
  s <- use stack
  if n > length s
  then err $ "Stack underflow, expected " ++ show n
  else do
    stack .= drop n s
    return (take n s)

byteCode :: VM m => m ByteCode
byteCode = do
  c <- use ctr
  p <- use prog
  return $ p V.! c

err :: VM m => String -> m a
err msg = do
  idx <- use ctr
  bc <- byteCode
  throwError $ msg ++ " (index " ++ show idx ++
          ", value " ++ show bc ++ ")"

forward :: VM m => m ByteCode
forward = do
  c <- use ctr
  p <- use prog
  if c + 1 > V.length p 
  then err "forward: EOF"
  else do
    ctr .= c + 1
    byteCode



data ControlFlow = 
          Next
        | Stop
        | Jump Int
    deriving (Show)


exec :: VM m => m ControlFlow
exec = do
  bc <- byteCode
  case bc of
    PushV _ -> err "Push value at top-level"
    Inst i -> do
            let (Spec _ stackin _ pspec) = spec i
            svals <- pops stackin
            dispatch i (pspec,svals) 
            
 
next :: VM m => m ControlFlow
next = return Next

pushb :: VM m => Bool -> m ()
pushb b = push $ if b then 1 else 0 

sgn :: U256 -> S256
sgn = fromIntegral

pushs :: VM m => S256 -> m ()
pushs = push . fromIntegral

dispatch :: VM m => Instruction -> (ParamSpec,[U256]) -> m ControlFlow
dispatch STOP _ = return Stop
dispatch _ (Push _,_) = do
  n <- forward
  case n of
    (PushV w) -> push w >> next
    _ -> err "Push: Instruction encountered"
dispatch ADD (_,[a,b]) = push (a + b) >> next
dispatch MUL (_,[a,b]) = push (a * b) >> next 
dispatch SUB (_,[a,b]) = push (a - b) >> next 
dispatch DIV (_,[a,b]) = 
    push (if b == 0 then 0 else a `div` b) >> next 
dispatch SDIV (_,[a,b]) = 
    pushs (if b == 0 then 0 else sgn a `div` sgn b) >> next
dispatch MOD (_,[a,b]) = push (a `mod` b) >> next 
dispatch SMOD (_,[a,b]) = pushs (sgn a `mod` sgn b) >> next
dispatch ADDMOD (_,[a,b,c]) = push (a + b `mod` c) >> next
dispatch MULMOD (_,[a,b,c]) = push (a * b `mod` c) >> next 
dispatch EXP (_,[a,b]) = push (a ^ b) >> next 
dispatch SIGNEXTEND _ = err "TODO" 
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
dispatch BYTE _ = err "TODO" 
dispatch SHA3 _ = err "TODO" 
dispatch ADDRESS _ = err "TODO" 
dispatch BALANCE _ = err "TODO" 
dispatch ORIGIN _ = err "TODO" 
dispatch CALLER _ = err "TODO" 
dispatch CALLVALUE _ = err "TODO" 
dispatch CALLDATALOAD _ = err "TODO" 
dispatch CALLDATASIZE _ = err "TODO" 
dispatch CALLDATACOPY _ = err "TODO" 
dispatch CODESIZE _ = err "TODO" 
dispatch CODECOPY _ = err "TODO" 
dispatch GASPRICE _ = err "TODO" 
dispatch EXTCODESIZE _ = err "TODO" 
dispatch EXTCODECOPY _ = err "TODO" 
dispatch BLOCKHASH _ = err "TODO" 
dispatch COINBASE _ = err "TODO" 
dispatch TIMESTAMP _ = err "TODO" 
dispatch NUMBER _ = err "TODO" 
dispatch DIFFICULTY _ = err "TODO" 
dispatch GASLIMIT _ = err "TODO" 
dispatch POP _ = err "TODO" 
dispatch MLOAD _ = err "TODO" 
dispatch MSTORE _ = err "TODO" 
dispatch MSTORE8 _ = err "TODO" 
dispatch SLOAD _ = err "TODO" 
dispatch SSTORE _ = err "TODO" 
dispatch JUMP _ = err "TODO" 
dispatch JUMPI _ = err "TODO" 
dispatch PC _ = err "TODO" 
dispatch MSIZE _ = err "TODO" 
dispatch GAS _ = err "TODO" 
dispatch JUMPDEST _ = err "TODO" 
dispatch _ (Dup n,_) = err "TODO" 
dispatch _ (Swap n,_) = err "TODO" 
dispatch _ (Log n,_) = err "TODO" 
dispatch CREATE _ = err "TODO" 
dispatch CALL _ = err "TODO" 
dispatch CALLCODE _ = err "TODO" 
dispatch RETURN _ = err "TODO" 
dispatch SUICIDE _ = err "TODO" 
dispatch _ ps = err $ "Unsupported operation [" ++ show ps ++ "]"
