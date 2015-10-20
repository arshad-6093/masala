{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Masala.VM.Dispatch where

import Masala.Instruction
import Masala.Word
import Masala.VM.Types
import Masala.VM.Memory
import Masala.VM.Gas
import Control.Monad
import Prelude hiding (LT,GT,EQ,log)
import Control.Lens
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Control.Monad.Except
import qualified Data.ByteArray as BA
import Crypto.Hash

infMod :: (Integer -> Integer -> Integer) -> U256 -> U256 -> U256 -> U256
infMod f a b c = fromIntegral ((fromIntegral a `f` fromIntegral b) `mod` fromIntegral c)

dispatch :: MonadExt m => Instruction -> (Maybe ParamSpec,[U256]) -> VM m ControlFlow
dispatch STOP _ = return Stop
dispatch ADD (_,[a,b]) = push (a + b) >> next
dispatch MUL (_,[a,b]) = push (a * b) >> next
dispatch SUB (_,[a,b]) = push (a - b) >> next
dispatch DIV (_,[a,b]) = push (if b == 0 then 0 else a `div` b) >> next
dispatch SDIV (_,[a,b]) = pushs (sgn a `sdiv` sgn b) >> next
dispatch MOD (_,[a,b]) = push (if b == 0 then 0 else a `mod` b) >> next
dispatch SMOD (_,[a,b]) = pushs (sgn a `smod` sgn b) >> next
dispatch ADDMOD (_,[a,b,c]) = push (if c == 0 then 0 else infMod (+) a b c) >> next
dispatch MULMOD (_,[a,b,c]) = push (if c == 0 then 0 else infMod (*) a b c) >> next
dispatch EXP (_,[a,b]) = push (a ^ b) >> next
dispatch SIGNEXTEND (_,[a,b]) = push (int a `signextend` b) >> next
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
dispatch BYTE (_,[a,b]) = push (int a `byte` b) >> next
dispatch SHA3 (_,[a,b]) = sha3 <$> mloads a b >>= push >> next
dispatch ADDRESS _ = view address >>= push . fromIntegral >> next
dispatch BALANCE (_,[a]) = maybe 0 (fromIntegral . view acctBalance) <$> addy (toAddress a) >>= push >> next
dispatch ORIGIN _ = view origin >>= push . fromIntegral >> next
dispatch CALLER _ = view caller >>= push . fromIntegral >> next
dispatch CALLVALUE _ = view callValue >>= push >> next
dispatch CALLDATALOAD (_,[a]) = callDataLoad a >>= push >> next
dispatch CALLDATASIZE _ = fromIntegral . V.length <$> view callData >>= push >> next
dispatch CALLDATACOPY (_,[a,b,c]) = V.toList <$> view callData >>=
                                    mstores a b c >> next
dispatch CODESIZE _ = fromIntegral . length . bcsToU8s . V.toList . pCode <$> view prog >>= push >> next
dispatch CODECOPY (_,[a,b,c]) = bcsToU8s . V.toList . pCode <$> view prog >>=
                                codeCopy a b c >> next
dispatch GASPRICE _ = view gasPrice >>= push >> next
dispatch EXTCODESIZE (_,[a]) =
    maybe 0 (fromIntegral . length . view acctCode) <$> addy (toAddress a) >>= push >> next
dispatch EXTCODECOPY (_,[a,b,c,d]) =
    maybe (bcsToU8s $ toByteCode STOP) (view acctCode) <$> addy (toAddress a) >>=
    codeCopy b c d >> next
dispatch BLOCKHASH (_,[a]) = view number >>= blockHash a >>= push >> next
dispatch COINBASE _ = view coinbase >>= push >> next
dispatch TIMESTAMP _ = view timestamp >>= push >> next
dispatch NUMBER _ = view number >>= push >> next
dispatch DIFFICULTY _ = view difficulty >>= push >> next
dispatch GASLIMIT _ = fromIntegral <$> view gaslimit >>= push >> next
dispatch POP _ = next -- exec already pops 1 per spec
dispatch MLOAD (_,[a]) = mload a >>= push >> next
dispatch MSTORE (_,[a,b]) = mstore a b >> next
dispatch MSTORE8 (_,[a,b]) = mstore8 (fromIntegral a) (fromIntegral b) >> next
dispatch SLOAD (_,[a]) = sload a >>= push >> next
dispatch SSTORE (_,[a,b]) = sstore a b >> next
dispatch JUMP (_,[a]) = jump a
dispatch JUMPI (_,[a,b]) = if b /= 0 then jump a else next
dispatch PC _ = fromIntegral . bcIdx <$> current >>= push >> next
dispatch MSIZE _ = msize >>= push >> next
dispatch GAS _ = fromIntegral <$> use gas >>= push >> next
dispatch JUMPDEST _ = next -- per spec: "Mark a valid destination for jumps."
                           -- "This operation has no effect on machine state during execution."
dispatch _ (Just (Dup n),_) = dup n >> next
dispatch _ (Just (Swap n),_) = swap n >> next
dispatch _ (Just (Log n),args) = log n args >> next
dispatch CREATE (_,[a,b,c]) = create (fromIntegral a) b (fromIntegral c)
dispatch CALL (_,[g,t,gl,io,il,oo,ol]) =
    let a = toAddress t in
    doCall (fromIntegral g) a a (fromIntegral gl) io il oo ol
dispatch CALLCODE (_,[g,t,gl,io,il,oo,ol]) =
    view address >>= \a ->
    doCall (fromIntegral g) a (toAddress t) (fromIntegral gl) io il oo ol
dispatch RETURN (_,[a,b]) = Return <$> mloads a b
dispatch SUICIDE (_,[a]) = suicide (toAddress a)
dispatch i ps = err $ "Unsupported operation/arity/spec: " ++ show i ++ ", " ++ show ps

callDataLoad :: Monad m => U256 -> VM m U256
callDataLoad i = do
  cd <- view callData
  if i > fromIntegral (V.length cd)
  then return 0
  else do
    let check [] = 0
        check (a:_) = a
    return . check . u8sToU256s . map (fromMaybe 0 . (cd V.!?)) $ [fromIntegral i .. fromIntegral i+31]


next :: Monad m => VM m ControlFlow
next = return Next

pushb :: Monad m => Bool -> VM m ()
pushb b = push $ if b then 1 else 0

sgn :: U256 -> S256
sgn = fromIntegral

pushs :: Monad m => S256 -> VM m ()
pushs = push . fromIntegral

int :: Integral a => a -> Int
int = fromIntegral

jump :: Monad m => U256 -> VM m ControlFlow
jump j = do
  bc <- M.lookup j . pCodeMap <$> view prog
  case bc of
    Nothing -> err $ "jump: invalid address " ++ show j
    Just c -> return (Jump c)

codeCopy :: Monad m => U256 -> U256 -> U256 -> [U8] -> VM m ()
codeCopy memloc codeoff len codes
    | codeoff +^ len > fromIntegral (length codes) = return ()
    | otherwise = mstores memloc 0 (fromIntegral $ length us) us
    where us = take (fromIntegral len) . drop (fromIntegral codeoff) $ codes

lookupAcct :: MonadExt m => String -> Address -> VM m ExtAccount
lookupAcct msg addr = do
  l <- extAddress addr
  maybe (throwError $ msg ++ ": " ++ show addr) return l

doCall :: MonadExt m => Gas -> Address -> Address -> Gas ->
          U256 -> U256 -> U256 -> U256 -> VM m ControlFlow
doCall cgas addr codeAddr cgaslimit inoff inlen outoff outlen = do
  d <- mloads inoff inlen
  codes <- view acctCode <$> lookupAcct "doCall: invalid code acct" codeAddr
  acct <- lookupAcct "doCall: invalid recipient acct" addr
  return $ Yield Call { cGas = cgas, cAcct = acct, cCode = codes, cGasLimit = cgaslimit,
                              cData = d, cAction = SaveMem outoff outlen }

create :: MonadExt m => Gas -> U256 -> U256 -> VM m ControlFlow
create cgas codeloc codeoff = do
  codes <- mloads codeloc codeoff
  newaddy <- extCreate cgas
  deductGas cgas
  gl <- view gaslimit
  return  $ Yield Call { cGas = cgas, cAcct = newaddy, cCode = codes, cGasLimit = gl,
                         cData = [], cAction = SaveCode (view acctAddress newaddy) }

suicide :: MonadExt m => Address -> VM m ControlFlow
suicide addr = do
  isNewSuicide <- extSuicide addr
  when isNewSuicide refundSuicide
  return Stop

addy :: MonadExt m => Address -> VM m (Maybe ExtAccount)
addy k = extAddress k


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


dup :: Monad m => Int -> VM m ()
dup n = stackAt (n - 1) >>= push

stackAt :: Monad m => Int -> VM m U256
stackAt n = do
  s <- firstOf (ix n) <$> use stack
  case s of
    Nothing -> err $ "stackAt " ++ show n ++ ": stack underflow"
    Just w -> return w

swap :: Monad m => Int -> VM m ()
swap n = do
  s0 <- stackAt 0
  sn <- stackAt n
  stack %= set (ix 0) sn . set (ix n) s0

log :: MonadExt m => Int -> [U256] -> VM m ()
log n (mstart:sz:topics)
    | length topics /= n =
        err $ "Dispatch error, LOG" ++ show n ++ " with " ++ show (length topics) ++ " topics"
    | otherwise = do
        a <- view address
        b <- view number
        d <- mloads mstart sz
        extLog $ LogEntry a b topics d
log n ws = err $ "Dispatch error LOG" ++ show n ++ ", expected 3 args: " ++ show ws

sdiv :: S256 -> S256 -> S256
sdiv a b | b == 0 = 0
         -- | a == bigneg || b == (-1) = bigneg -- go code doesn't like this apparently
         | otherwise = (abs a `div` abs b) * (signum a * signum b)
         -- where bigneg = (-2) ^ (255 :: Int)

smod :: S256 -> S256 -> S256
smod a b | b == 0 = 0
         | otherwise = (abs a `mod` abs b) * signum a


err :: Monad m => String -> VM m a
err msg = do
  idx <- use ctr
  bc <- current
  throwError $ msg ++ " (index " ++ show idx ++
          ", value " ++ show bc ++ ")"

current :: Monad m => VM m ByteCode
current = do
  c <- use ctr
  (Prog p _) <- view prog
  return $ p V.! c

push :: (Monad m) => U256 -> VM m ()
push i = stack %= (i:)

pops :: (Monad m) => Int -> VM m [U256]
pops n | n == 0 = return []
       | otherwise = do
  s <- use stack
  if n > length s
  then err $ "Stack underflow, expected " ++ show n ++ ", found " ++ show (length s)
  else do
    stack .= drop n s
    return (take n s)



sha3 :: [U8] -> U256
sha3 = head . u8sToU256s . map fromIntegral . BA.unpack .
       (hash :: BA.Bytes -> Digest Kekkak_256) . BA.pack . map fromIntegral

blockHash :: Monad m => U256 -> U256 -> VM m U256
blockHash n blocknum = return $ sha3 $ u256ToU8s (n + blocknum)
