{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Masala.VM.Dispatch where

import Masala.Instruction
import Masala.Word
import Masala.VM.Types
import Masala.Ext
import Control.Monad
import Prelude hiding (LT,GT,EQ,log)
import Control.Lens
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Control.Monad.Except
import Masala.Gas
import qualified Data.ByteArray as BA
import Crypto.Hash

infMod :: (Integer -> Integer -> Integer) -> U256 -> U256 -> U256 -> U256
infMod f a b c = fromIntegral ((fromIntegral a `f` fromIntegral b) `mod` fromIntegral c)

dispatch :: Instruction -> (Maybe ParamSpec,[U256]) -> VM e (ControlFlow e)
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
dispatch CALLDATALOAD (_,[a]) = callDataLoad (int a) >>= push >> next
dispatch CALLDATASIZE _ = fromIntegral . V.length <$> view callData >>= push >> next
dispatch CALLDATACOPY (_,[a,b,c]) = V.toList <$> view callData >>=
                                    mstores a b c >> next
dispatch CODESIZE _ = fromIntegral . V.length . pCode <$> view prog >>= push >> next
dispatch CODECOPY (_,[a,b,c]) = bcsToU8s . V.toList . pCode <$> view prog >>=
                                codeCopy a (int b) (int c) >> next
dispatch GASPRICE _ = view gasPrice >>= push >> next
dispatch EXTCODESIZE (_,[a]) =
    maybe 0 (fromIntegral . length . view acctCode) <$> addy (toAddress a) >>= push >> next
dispatch EXTCODECOPY (_,[a,b,c,d]) =
    maybe (bcsToU8s $ toByteCode STOP) (view acctCode) <$> addy (toAddress a) >>=
    codeCopy b (int c) (int d) >> next
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
dispatch PC _ = fromIntegral <$> use ctr >>= push >> next
dispatch MSIZE _ = fromIntegral . (* 8) . M.size <$> use mem >>= push >> next
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


callDataLoad :: Int -> VM e U256
callDataLoad i = do
  cd <- view callData
  let check [] = 0
      check (a:_) = a
  return . check . u8sToU256s . map (fromMaybe 0 . (cd V.!?)) $ [i .. i+31]


next :: VM e (ControlFlow e)
next = return Next

pushb :: Bool -> VM e ()
pushb b = push $ if b then 1 else 0

sgn :: U256 -> S256
sgn = fromIntegral

pushs :: S256 -> VM e ()
pushs = push . fromIntegral

int :: Integral a => a -> Int
int = fromIntegral

jump :: U256 -> VM e (ControlFlow e)
jump j = do
  bc <- M.lookup j . pCodeMap <$> view prog
  case bc of
    Nothing -> err $ "jump: invalid address " ++ show j
    Just c -> return (Jump c)

codeCopy :: U256 -> Int -> Int -> [U8] -> VM e ()
codeCopy memloc codeoff len codes = mstores memloc 0 (fromIntegral $ length us) us
    where us = take len . drop codeoff $ codes

lookupAcct :: String -> Address -> VM e ExtAccount
lookupAcct msg addr = do
  l <- xRun $ xAddress <@$> addr
  maybe (throwError $ msg ++ ": " ++ show addr) return l

doCall :: Gas -> Address -> Address -> Gas ->
          U256 -> U256 -> U256 -> U256 -> VM e (ControlFlow e)
doCall cgas addr codeAddr cgaslimit inoff inlen outoff outlen = do
  d <- mloads inoff inlen
  codes <- view acctCode <$> lookupAcct "doCall: invalid code acct" codeAddr
  acct <- lookupAcct "doCall: invalid recipient acct" addr
  return $ Yield Call { cGas = cgas, cAcct = acct, cCode = codes, cGasLimit = cgaslimit,
                              cData = d, cAction = SaveMem outoff outlen }

create :: Gas -> U256 -> U256 -> VM e (ControlFlow e)
create cgas codeloc codeoff = do
  codes <- mloads codeloc codeoff
  newaddy <- xRun $ xCreate <@$> cgas
  deductGas cgas
  gl <- view gaslimit
  return  $ Yield Call { cGas = cgas, cAcct = newaddy, cCode = codes, cGasLimit = gl,
                         cData = [], cAction = SaveCode (view acctAddress newaddy) }

suicide :: Address -> VM e (ControlFlow e)
suicide addr = do
  isNewSuicide <- xRun $ xSuicide <@$> addr
  when isNewSuicide $ refund gas_suicide
  return Stop

addy :: Address -> VM e (Maybe ExtAccount)
addy k = xRun $ xAddress <@$> k

mload :: U256 -> VM e U256
mload i = do
  m <- use mem
  return $ head . u8sToU256s . map (fromMaybe 0 . (`M.lookup` m)) $ [i .. i+31]


mstore :: U256 -> U256 -> VM e ()
mstore i v = mem %= M.union (M.fromList $ reverse $ zip (reverse [i .. i + 31]) (reverse (u256ToU8s v) ++ repeat 0))

mstore8 :: U256 -> U8 -> VM e ()
mstore8 i b = mem %= M.insert i b


mloads :: U256 -> U256 -> VM e [U8]
mloads loc len | len < 1 = return []
               | otherwise = do
  m <- use mem
  return $ map (fromMaybe 0 . (`M.lookup` m)) $ [loc .. loc + len - 1]


mstores :: U256 -> U256 -> U256 -> [U8] -> VM e ()
mstores memloc off len v
    | len < 1 = return ()
    | otherwise = mem %= M.union (M.fromList $ zip [memloc .. memloc + len - 1] (drop (fromIntegral off) v))



sload :: U256 -> VM e U256
sload i = do
  s <- view address
  fromMaybe 0 <$> xRun (xLoad <@$> s <@*> i)

sstore :: U256 -> U256 -> VM e ()
sstore a b = do
  s <- view address
  xRun $ xStore <@$> s <@*> a <@*> b


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


dup :: Int -> VM e ()
dup n = stackAt (n - 1) >>= push

stackAt :: Int -> VM e U256
stackAt n = do
  s <- firstOf (ix n) <$> use stack
  case s of
    Nothing -> err $ "stackAt " ++ show n ++ ": stack underflow"
    Just w -> return w

swap :: Int -> VM e ()
swap n = do
  s0 <- stackAt 0
  sn <- stackAt n
  stack %= set (ix 0) sn . set (ix n) s0

log :: Int -> [U256] -> VM e ()
log n (mstart:msize:topics)
    | length topics /= n =
        err $ "Dispatch error, LOG" ++ show n ++ " with " ++ show (length topics) ++ " topics"
    | otherwise = do
        a <- view address
        b <- view number
        d <- mloads mstart msize
        xRun $ xLog <@$> LogEntry a b topics d
log n ws = err $ "Dispatch error LOG" ++ show n ++ ", expected 3 args: " ++ show ws

sdiv :: S256 -> S256 -> S256
sdiv a b | b == 0 = 0
         -- | a == bigneg || b == (-1) = bigneg -- go code doesn't like this apparently
         | otherwise = (abs a `div` abs b) * (signum a * signum b)
         -- where bigneg = (-2) ^ (255 :: Int)

smod :: S256 -> S256 -> S256
smod a b | b == 0 = 0
         | otherwise = (abs a `mod` abs b) * signum a

deductGas :: Gas -> VM e ()
deductGas total = do
  enabled <- view doGas
  when enabled $ do
    pg <- use gas
    let gas' = pg - total
    if gas' < 0
    then do
      gas .= 0
      throwError $ "Out of gas, gas=" ++ show pg ++
                 ", required=" ++ show total ++
                 ", balance= " ++ show gas'
    else do
      d <- view debug
      when d $ liftIO $ putStrLn $ "gas used: " ++ show total
      gas .= gas'

err :: String -> VM e a
err msg = do
  idx <- use ctr
  bc <- current
  throwError $ msg ++ " (index " ++ show idx ++
          ", value " ++ show bc ++ ")"

current :: VM e ByteCode
current = do
  c <- use ctr
  (Prog p _) <- view prog
  return $ p V.! c

push :: U256 -> VM e ()
push i = stack %= (i:)

pops :: Int -> VM e [U256]
pops n | n == 0 = return []
       | otherwise = do
  s <- use stack
  if n > length s
  then err $ "Stack underflow, expected " ++ show n ++ ", found " ++ show (length s)
  else do
    stack .= drop n s
    return (take n s)

refund :: Gas -> VM e ()
refund g = do
  a <- view address
  xRun $ xRefund <@$> a <@*> g


sha3 :: [U8] -> U256
sha3 = head . u8sToU256s . map fromIntegral . BA.unpack .
       (hash :: BA.Bytes -> Digest Kekkak_256) . BA.pack . map fromIntegral

blockHash :: U256 -> U256 -> VM e U256
blockHash n blocknum = return $ sha3 $ u256ToU8s (n + blocknum)
