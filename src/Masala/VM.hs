{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Masala.VM where

import Data.Bits
import Data.Word
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Lens
import Masala.Instruction
import Control.Monad.Error
import qualified Data.Vector as V
import Control.Applicative
import Prelude hiding (LT,GT,EQ,log)
import Numeric
import Data.Char (intToDigit)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid


data Prog = Prog {
      code :: V.Vector ByteCode
    , codeMap :: M.Map U256 Int
}

type Stack = [U256]
type Mem = M.Map U256 U256
type Ctr = Int
data VMState e = VMState {
      _stack :: Stack
    , _ctr :: Ctr
    , _mem :: Mem
    , _ext :: e
} deriving (Eq,Show)

data ExtAccount = ExtAccount {
      acctCode :: [Word8]
    } deriving (Eq,Show)

data Ext e = Ext {
      xStore :: U256 -> U256 -> e -> e
    , xLoad :: U256 -> e -> Maybe U256
    , xAddress :: U256 -> e -> Maybe ExtAccount
}

data Env e = Env {
      debug :: Bool
    , callData :: V.Vector U256
    , extApi :: Ext e
    , prog :: Prog
    , address :: U256
    , origin, caller :: U256
    , balance, envGas, gasPrice, callValue :: U256
    , prevHash, coinbase, timestamp :: U256
    , number, difficulty, gaslimit :: U256
}

data Output = Output { oValue :: [Word8] } deriving (Eq,Show)


type VM m e = (Functor m, Monad m, Applicative m,
             MonadIO m,
             MonadState (VMState e) m,
             MonadError String m,
             MonadReader (Env e) m)


data ControlFlow =
          Next
        | Stop
        | Jump Int
        | Return [Word8]
    deriving (Show)

stack :: Lens' (VMState e) Stack
stack f s = fmap (\a -> s { _stack = a }) (f $ _stack s)
ctr :: Lens' (VMState e) Ctr
ctr f s = fmap (\a -> s { _ctr = a }) (f $ _ctr s)
mem :: Lens' (VMState e) Mem
mem f s = fmap (\a -> s { _mem = a }) (f $ _mem s)
ext :: Lens' (VMState e) e
ext f s = fmap (\a -> s { _ext = a }) (f $ _ext s)


toProg :: [ByteCode] -> Prog
toProg bc = Prog (V.fromList bc) (M.fromList (zipWith idx [0..] bc))
    where idx c (ByteCode n _ _) = (fromIntegral n,c)


push :: (VM m e) => U256 -> m ()
push i = stack %= (i:)


pops :: Int -> VM m e => m [U256]
pops n | n == 0 = return []
       | otherwise = do
  s <- use stack
  if n > length s
  then err $ "Stack underflow, expected " ++ show n ++ "," ++ show (length s)
  else do
    stack .= drop n s
    return (take n s)

current :: VM m e => m ByteCode
current = do
  c <- use ctr
  (Prog p _) <- reader prog
  return $ p V.! c


err :: VM m e => String -> m a
err msg = do
  idx <- use ctr
  bc <- current
  throwError $ msg ++ " (index " ++ show idx ++
          ", value " ++ show bc ++ ")"

forward :: VM m e => m Bool
forward = do
  c <- use ctr
  (Prog p _) <- reader prog
  if c + 1 >= V.length p
  then return False
  else do
    ctr .= c + 1
    return True


runVM :: (MonadIO m, Functor m, Show ext) => ext -> Env ext -> m (Either String Output)
runVM extState env = evalStateT (runReaderT (runErrorT go) env)
                    (VMState [] 0 M.empty extState)
    where go = do
            cf <- exec
            case cf of
              Next -> do
                      notDone <- forward
                      if notDone
                      then go
                      else done []
                        
              Jump c -> do
                      ctr .= c
                      go
              Stop -> done []
              Return ws -> done ws
          done ws = do
                 d <- reader debug
                 vm <- get
                 when d $ liftIO $ print vm
                 return (Output ws)

run_ :: String -> IO (Either String Output)
run_ = runBC_ . parseHex

runBC_ :: ToByteCode a => [a] -> IO (Either String Output)
runBC_ bc = runVM (TestExtData M.empty M.empty)
            (Env True (V.fromList [1,2,3,4,5]) testExt
             (toProg (concatMap toByteCode bc))
             0 0 0 0 0 0 0 0 0 0 0 0 0)

data TestExtData = TestExtData {
      _edStore :: M.Map U256 U256
    , _edAccts :: M.Map U256 ExtAccount
} deriving (Eq,Show)

edStore :: Lens' TestExtData (M.Map U256 U256)
edStore f s = fmap (\a -> s { _edStore = a }) (f $ _edStore s)
edAccts :: Lens' TestExtData (M.Map U256 ExtAccount)
edAccts f s = fmap (\a -> s { _edAccts = a }) (f $ _edAccts s)

testExt :: Ext TestExtData
testExt = Ext {
            xStore = \k v -> over edStore (M.insert k v)
          , xLoad = \k -> firstOf (edStore . ix k)
          , xAddress = \k -> firstOf (edAccts . ix k)
          }


bin_ :: (Show a, Integral a) => a -> String
bin_ i = showIntAtBase 2 intToDigit i ""

exec :: (Show e, VM m e) => m ControlFlow
exec = do
  bc@(ByteCode _ i ws) <- current
  let (Spec _ stackin _ pspec) = spec i
  svals <- pops stackin
  d <- reader debug
  when d $ debugOut bc svals
  if null ws 
  then dispatch i (pspec,svals)
  else mapM_ push (w8sToU256s ws) >> next
    

debugOut :: (Show e, VM m e) => ByteCode -> [U256] -> m ()
debugOut i svals = do
  vm <- get
  liftIO $ print (i,svals,vm)

next :: VM m e => m ControlFlow
next = return Next

pushb :: VM m e => Bool -> m ()
pushb b = push $ if b then 1 else 0

sgn :: U256 -> S256
sgn = fromIntegral

pushs :: VM m e => S256 -> m ()
pushs = push . fromIntegral



dup :: VM m e => Int -> m ()
dup n = stackAt (n - 1) >>= push

stackAt :: VM m e => Int -> m U256
stackAt n = do
  s <- firstOf (ix n) <$> use stack
  case s of
    Nothing -> err $ "stackAt " ++ show n ++ ": stack underflow"
    Just w -> return w

swap :: VM m e => Int -> m ()
swap n = do
  s0 <- stackAt 0
  sn <- stackAt n
  stack %= set (ix 0) sn . set (ix n) s0

log :: VM m e => Int -> m ()
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

mload :: VM m e => U256 -> m U256
mload i = do
  mv <- M.lookup i <$> use mem
  case mv of
    Just v -> return v
    Nothing -> mem %= M.insert i 0 >> return 0

mstore :: VM m e => U256 -> U256 -> m ()
mstore i v = mem %= M.insert i v


sload :: VM m e => U256 -> m U256
sload i = do
  f <- xLoad <$> reader extApi
  fromMaybe 0 . f i <$> use ext

sstore :: VM m e => U256 -> U256 -> m ()
sstore a b = do
  f <- xStore <$> reader extApi
  ext %= f a b

toAddy :: U256 -> U256
toAddy = (`mod` (2 ^ (160 :: Int)))

addy :: VM m e => U256 -> m (Maybe ExtAccount)
addy k = do
  f <- xAddress <$> reader extApi
  f (toAddy k) <$> use ext 

copyMem :: VM m e => U256 -> Int -> Int -> V.Vector U256 -> m ()
copyMem memloc off len v
    | V.length v < off + len = return ()
    | otherwise =
        mem %= mappend (M.fromList . zip [memloc ..] . V.toList . V.slice off len $ v)

getMem :: VM m e => U256 -> U256 -> m [Word8]
getMem loc len | len > 0 = concatMap u256ToW8s <$> mapM mload [loc .. loc + (len - 1)]
    

jump :: VM m e => U256 -> m ControlFlow
jump j = do
  bc <- M.lookup j . codeMap <$> reader prog
  case bc of
    Nothing -> err $ "jump: invalid address " ++ show j
    Just c -> return (Jump c) 

codeCopy :: VM m e => U256 -> Int -> Int -> [Word8] -> m ()
codeCopy memloc codeoff len codes = copyMem memloc 0 (V.length us) us
    where us = V.fromList . w8sToU256s . take len . drop codeoff $ codes
            
        

dispatch :: VM m e => Instruction -> (ParamSpec,[U256]) -> m ControlFlow
dispatch STOP _ = return Stop
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
dispatch CALLDATALOAD (_,[a]) = fromMaybe 0 . (V.!? fromIntegral a) <$> reader callData >>= 
                                push >> next
dispatch CALLDATASIZE _ = fromIntegral . V.length <$> reader callData >>= push >> next
dispatch CALLDATACOPY (_,[a,b,c]) = reader callData >>=
                                    copyMem a (fromIntegral b) (fromIntegral c) >> next
dispatch CODESIZE _ = fromIntegral . V.length . code <$> reader prog >>= push >> next
dispatch CODECOPY (_,[a,b,c]) = bcsToWord8s . V.toList . code <$> reader prog >>= 
                                codeCopy a (fromIntegral b) (fromIntegral c) >> next
dispatch GASPRICE _ = reader gasPrice >>= push >> next
dispatch EXTCODESIZE (_,[a]) = maybe 0 (fromIntegral . length . acctCode) <$> addy a >>= 
                               push >> next
dispatch EXTCODECOPY (_,[a,b,c,d]) = maybe (bcsToWord8s $ toByteCode STOP) acctCode <$> addy a >>= 
                                     codeCopy b (fromIntegral c) (fromIntegral d) >> next 
dispatch BLOCKHASH _ = err "TODO"
dispatch COINBASE _ = reader coinbase >>= push >> next
dispatch TIMESTAMP _ = reader timestamp >>= push >> next
dispatch NUMBER _ = reader number >>= push >> next
dispatch DIFFICULTY _ = reader difficulty >>= push >> next
dispatch GASLIMIT _ = reader gaslimit >>= push >> next
dispatch POP _ = next -- exec already pops 1 per spec
dispatch MLOAD (_,[a]) = mload a >>= push >> next
dispatch MSTORE (_,[a,b]) = mstore a b >> next
dispatch MSTORE8 (_,[a,b]) = mstore a b >> next
dispatch SLOAD (_,[a]) = sload a >>= push >> next
dispatch SSTORE (_,[a,b]) = sstore a b >> next
dispatch JUMP (_,[a]) = jump a
dispatch JUMPI (_,[a,b]) = if b /= 0 then jump a else next
dispatch PC _ = fromIntegral <$> use ctr >>= push >> next
dispatch MSIZE _ = fromIntegral . M.size <$> use mem >>= push >> next
dispatch GAS _ = err "TODO"
dispatch JUMPDEST _ = next -- per spec: "Mark a valid destination for jumps."
                           -- "This operation has no effect on machine state during execution."
dispatch _ (Dup n,_) = dup n >> next
dispatch _ (Swap n,_) = swap n >> next
dispatch _ (Log n,_) = log n >> next
dispatch CREATE _ = err "TODO"
dispatch CALL _ = err "TODO"
dispatch CALLCODE _ = err "TODO"
dispatch RETURN (_,[a,b]) = Return <$> getMem (a `div` 32) (b `div` 32)
dispatch SUICIDE _ = err "TODO"
dispatch _ ps = err $ "Unsupported operation [" ++ show ps ++ "]"
