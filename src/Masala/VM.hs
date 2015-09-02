{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module Masala.VM where

import Data.Bits
import Data.Word
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Lens hiding (op)
import Masala.Instruction
import Control.Monad.Except
import qualified Data.Vector as V
import Control.Applicative
import Prelude hiding (LT,GT,EQ,log)
import Numeric
import Data.Char (intToDigit)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Masala.Gas
import qualified Data.Set as S


data Prog = Prog {
      -- parsed bytecode
      pCode :: V.Vector ByteCode
      -- map of valid codepoints to pCode indexes
    , pCodeMap :: M.Map U256 Int
}

type Stack = [U256]
type Mem = M.Map U256 U256
type Ctr = Int
data VMState e = VMState {
      _stack :: Stack
    , _ctr :: Ctr
    , _mem :: Mem
    , _ext :: e
    , _gas :: Gas
} deriving (Eq,Show)

stack :: Lens' (VMState e) Stack
stack f s = fmap (\a -> s { _stack = a }) (f $ _stack s)
ctr :: Lens' (VMState e) Ctr
ctr f s = fmap (\a -> s { _ctr = a }) (f $ _ctr s)
mem :: Lens' (VMState e) Mem
mem f s = fmap (\a -> s { _mem = a }) (f $ _mem s)
ext :: Lens' (VMState e) e
ext f s = fmap (\a -> s { _ext = a }) (f $ _ext s)
gas :: Lens' (VMState e) Gas
gas f s = fmap (\a -> s { _gas = a }) (f $ _gas s)

data ExtAccount = ExtAccount {
      _acctCode :: [Word8]
    , _acctBalance :: Gas
    , _acctAddress :: U256
    , _acctStore :: M.Map U256 U256
    } deriving (Eq,Show)

acctStore :: Lens' ExtAccount (M.Map U256 U256)
acctStore f s = fmap (\a -> s { _acctStore = a }) (f $ _acctStore s)
acctCode :: Lens' ExtAccount [Word8]
acctCode f s = fmap (\a -> s { _acctCode = a }) (f $ _acctCode s)
acctBalance :: Lens' ExtAccount Gas
acctBalance f s = fmap (\a -> s { _acctBalance = a }) (f $ _acctBalance s)
acctAddress :: Lens' ExtAccount U256
acctAddress f s = fmap (\a -> s { _acctAddress = a }) (f $ _acctAddress s)

newtype ExtOp e a = ExtOp { runExtOp :: e -> (a, e) } deriving (Functor)
instance Applicative (ExtOp e) where
    pure a = ExtOp (a,)
    (ExtOp f) <*> (ExtOp a) = ExtOp $ \e -> ((fst $ f e) (fst $ a e),e)
instance Monad (ExtOp e) where
    return = pure
    (ExtOp a) >>= f = ExtOp $ \e -> runExtOp (f (fst $ a e)) e


execExtOp :: ExtOp e a -> e -> e
execExtOp op = snd . runExtOp op

evalExtOp :: ExtOp e a -> e -> a
evalExtOp op = fst . runExtOp op

setExt :: (e -> e) -> ExtOp e ()
setExt f = ExtOp $ ((),) . f

useExt :: (e -> a) -> ExtOp e a
useExt f = fmap f getExt

getExt :: ExtOp e e
getExt = ExtOp $ \e -> (e,e)

xover :: Profunctor p => Setting p s s a b -> p a b -> ExtOp s ()
xover l f = setExt $ over l f

xfirstOf :: Getting (Leftmost a) s a -> ExtOp s (Maybe a)
xfirstOf l = useExt $ firstOf l

xview :: Getting b s b -> ExtOp s b
xview l = view l <$> getExt

xApply :: VM m e => (Ext e -> f) -> (f -> ExtOp e a) -> m a
xApply acc g = do
  f <- acc <$> reader extApi
  (a,e) <- runExtOp (g f) <$> use ext
  ext .= e
  return a

-- | work in progress to use applicative??
appEx :: VM m e => (Env e -> a) -> m (ExtOp e a)
appEx acc = pure <$> reader acc







data Ext e = Ext {
      xStore :: U256 -> U256 -> U256 -> ExtOp e ()
    , xLoad :: U256 -> U256 -> ExtOp e (Maybe U256)
    , xAddress :: U256 -> ExtOp e (Maybe ExtAccount)
    , xCreate :: Gas -> ExtOp e ExtAccount
    , xSaveCode :: U256 -> [Word8] -> ExtOp e ()
    , xSuicide :: U256 -> ExtOp e Bool
    , xRefund :: Gas -> ExtOp e ()
    , xIsCreate :: U256 -> ExtOp e Bool
}

data Resume e = Resume {
      rPush :: U256,
      rResult ::[Word8],
      rAction :: CallAction,
      rExt :: e
    } deriving (Eq,Show)


data Env e = Env {
      debug :: Bool
    , doGas :: Bool
    , callData :: V.Vector U256
    , extApi :: Ext e
    , prog :: Prog
    , address :: U256
    , origin :: U256
    , caller :: U256
    , envGas :: U256
    , gasPrice :: U256
    , callValue :: U256
    , prevHash :: U256
    , coinbase :: U256
    , timestamp :: U256
    , number :: U256
    , difficulty :: U256
    , gaslimit :: Gas
}

data CallAction = SaveMem U256 Int | SaveCode U256 deriving (Eq,Show)

data VMResult =
    Final { fReturn :: [Word8] }
        | Call {
            cGas :: Gas,
            cAcct :: ExtAccount,
            cCode :: [Word8],
            cGasLimit :: Gas,
            cData :: [U256],
            cAction :: CallAction }
          deriving (Eq,Show)

type Output e = (VMResult, VMState e)


type VM m e = (Functor m, Monad m, Applicative m,
             MonadIO m,
             MonadState (VMState e) m,
             MonadError String m,
             MonadReader (Env e) m)


data ControlFlow e =
          Next
        | Stop
        | Jump Int
        | Return [Word8]
        | Yield VMResult
    deriving (Show)




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

emptyState :: e -> Gas -> VMState e
emptyState = VMState [] 0 M.empty

runVM :: (MonadIO m, Functor m, Show ext) =>
         VMState ext -> Env ext -> Maybe (Resume ext) -> m (Either String (Output ext))
runVM vm env callR = evalStateT (runReaderT (runExceptT go) env) vm >>= postEx env
    where go = (,) <$> stepVM callR <*> get


postEx :: (MonadIO m, Functor m, Show ext) =>
          Env ext -> Either String (Output ext) -> m (Either String (Output ext))
postEx _ l@(Left _) = return l
postEx _ r@(Right (Final {},_)) = return r
postEx env (Right (Call g addr codes glimit cdata action, vm)) = do
  let es = _ext vm
      env' = env {
               prog = toProg (parse codes)
             , address = _acctAddress addr
             , caller = address env
             , callData = V.fromList cdata
             }
  r <- runVM (emptyState es (fromIntegral g)) env' Nothing
  case r of
    Left _ -> return r
    (Right (Call {},_)) -> return $ Left $ "VM error: Call returned from 'runVM': " ++ show r
    (Right (Final o,vm')) ->
           runVM vm env (Just $ Resume 1 o action (_ext vm'))


stepVM :: (Show e, VM m e) => Maybe (Resume e) -> m VMResult
stepVM r = do
  let done ws = do
             doDebug (get >>= liftIO . print)
             return (Final ws)
  cf <- case r of
          Nothing -> exec
          (Just rs@(Resume p result action e)) -> do
              doDebug (liftIO $ putStrLn $ "Resume: " ++ show rs)
              ext .= e
              case action of
                SaveMem loc len -> mstores loc 0 len (V.fromList $ w8sToU256s result)
                SaveCode addr -> xApply xSaveCode (\f -> f addr result)
              push p
              return Next
  case cf of
    Next -> do
            notDone <- forward
            if notDone
            then stepVM Nothing
            else done []
    Jump c -> do
            ctr .= c
            stepVM Nothing
    Stop -> done []
    Return ws -> done ws
    Yield call -> do
             doDebug (liftIO $ putStrLn $ "Yield: " ++ show call)
             return call





bin_ :: (Show a, Integral a) => a -> String
bin_ i = showIntAtBase 2 intToDigit i ""

exec :: (Show e, VM m e) => m (ControlFlow e)
exec = do
  bc@(ByteCode _ i ws) <- current
  let (Spec _ stackin _ pspec) = spec i
  svals <- pops stackin
  doDebug $ debugOut bc svals
  handleGas i pspec svals
  if null ws
  then dispatch i (pspec,svals)
  else mapM_ push (w8sToU256s ws) >> next

handleGas :: VM m e => Instruction -> ParamSpec -> [U256] -> m ()
handleGas i ps svs = do
  let (callg,a) = computeGas i (ps,svs)
  calcg <- case a of
            Nothing -> return 0
            (Just c) -> case c of
                        (MemSize sz) -> computeMemGas sz
                        (StoreOp loc off) -> computeStoreGas loc off
                        (GasCall sz addr) -> (+) <$> computeMemGas sz <*> computeCallGas addr
  deductGas (calcg + callg)

deductGas :: VM m e => Gas -> m ()
deductGas total = do
  enabled <- reader doGas
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
      d <- reader debug
      when d $ liftIO $ putStrLn $ "gas used: " ++ show total
      gas .= gas'

computeMemGas :: VM m e => U256 -> m Gas
computeMemGas newSzBytes = do
  let toWordSize v = (v + 31) `div` 32
      newSzWords = fromIntegral $ toWordSize newSzBytes
      fee s = ((s * s) `div` 512) + (s * gas_memory)
  oldSzWords <- M.size <$> use mem
  return $ if newSzWords > oldSzWords
           then fee newSzWords - fee oldSzWords
           else 0


refund :: VM m e => Gas -> m ()
refund g = xApply xRefund ($ g)

computeStoreGas :: VM m e => U256 -> U256 -> m Gas
computeStoreGas l v' = do
  v <- mload l
  if v == 0 && v' /= 0
  then return gas_sset
  else if v /= 0 && v' == 0
       then refund gas_sclear >> return gas_sreset
       else return gas_sreset


computeCallGas :: VM m e => Maybe U256 -> m Gas
computeCallGas Nothing = return 0
computeCallGas (Just a) = do
  isNew <- xApply xIsCreate ($ a)
  return $ if isNew then gas_callnewaccount else 0


doDebug :: VM m e => m () -> m ()
doDebug a = do
  d <- reader debug
  when d a

debugOut :: (Show e, VM m e) => ByteCode -> [U256] -> m ()
debugOut i svals = do
  vm <- get
  liftIO $ print (i,svals,vm)

next :: VM m e => m (ControlFlow e)
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
mload i = fromMaybe 0 . M.lookup i <$> use mem

mstore :: VM m e => U256 -> U256 -> m ()
mstore i v = mem %= M.insert i v

mloads :: VM m e => U256 -> U256 -> m [U256]
mloads loc len | len == 0 = return []
               | otherwise = mapM mload [loc .. loc + (len - 1)]

mstores :: VM m e => U256 -> Int -> Int -> V.Vector U256 -> m ()
mstores memloc off len v
    | V.length v < off + len = return () -- really an error ...
    | otherwise =
        mem %= mstores' memloc off len v

mstores' :: U256 -> Int -> Int -> V.Vector U256 -> Mem -> Mem
mstores' memloc off len v =
    mappend (M.fromList . zip [memloc ..] . V.toList . V.slice off len $ v)


sload :: VM m e => U256 -> m U256
sload i = do
  s <- reader address
  fromMaybe 0 <$> xApply xLoad (\f -> f s i)

sstore :: VM m e => U256 -> U256 -> m ()
sstore a b = do
  s <- reader address
  xApply xStore (\f -> f s a b)

toAddy :: U256 -> U256
toAddy = (`mod` (2 ^ (160 :: Int)))

addy :: VM m e => U256 -> m (Maybe ExtAccount)
addy k = xApply xAddress ($ toAddy k)


int :: Integral a => a -> Int
int = fromIntegral

jump :: VM m e => U256 -> m (ControlFlow e)
jump j = do
  bc <- M.lookup j . pCodeMap <$> reader prog
  case bc of
    Nothing -> err $ "jump: invalid address " ++ show j
    Just c -> return (Jump c)

codeCopy :: VM m e => U256 -> Int -> Int -> [Word8] -> m ()
codeCopy memloc codeoff len codes = mstores memloc 0 (V.length us) us
    where us = V.fromList . w8sToU256s . take len . drop codeoff $ codes

lookupAcct :: VM m e => String -> U256 -> m ExtAccount
lookupAcct msg addr = do
  l <- xApply xAddress ($ addr)
  maybe (throwError $ msg ++ ": " ++ show addr) return l

doCall :: VM m e => Gas -> U256 -> U256 -> Gas ->
          U256 -> U256 -> U256 -> Int -> m (ControlFlow e)
doCall cgas addr codeAddr cgaslimit inoff inlen outoff outlen = do
  d <- mloads inoff inlen
  codes <- view acctCode <$> lookupAcct "doCall: invalid code acct" codeAddr
  acct <- lookupAcct "doCall: invalid recipient acct" addr
  return $ Yield Call { cGas = cgas, cAcct = acct, cCode = codes, cGasLimit = cgaslimit,
                              cData = d, cAction = SaveMem outoff outlen }


create :: VM m e => Gas -> U256 -> U256 -> m (ControlFlow e)
create cgas codeloc codeoff = do
  codes <- concatMap u256ToW8s <$> mloads codeloc codeoff
  newaddy <- xApply xCreate ($ cgas)
  deductGas cgas
  gl <- reader gaslimit
  return  $ Yield Call { cGas = cgas, cAcct = newaddy, cCode = codes, cGasLimit = gl,
                         cData = [], cAction = SaveCode (view acctAddress newaddy) }

suicide :: VM m e => U256 -> m (ControlFlow e)
suicide addr = do
  isNewSuicide <- xApply xSuicide ($ addr)
  when isNewSuicide $ refund gas_suicide
  return Stop

dispatch :: VM m e => Instruction -> (ParamSpec,[U256]) -> m (ControlFlow e)
dispatch STOP _ = return Stop
dispatch ADD (_,[a,b]) = push (a + b) >> next
dispatch MUL (_,[a,b]) = push (a * b) >> next
dispatch SUB (_,[a,b]) = push (a - b) >> next
dispatch DIV (_,[a,b]) = push (if b == 0 then 0 else a `div` b) >> next
dispatch SDIV (_,[a,b]) = pushs (sgn a `sdiv` sgn b) >> next
dispatch MOD (_,[a,b]) = push (a `mod` b) >> next
dispatch SMOD (_,[a,b]) = pushs (sgn a `mod` sgn b) >> next
dispatch ADDMOD (_,[a,b,c]) = push (a + b `mod` c) >> next
dispatch MULMOD (_,[a,b,c]) = push (a * b `mod` c) >> next
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
dispatch SHA3 _ = err "TODO"
dispatch ADDRESS _ = reader address >>= push >> next
dispatch BALANCE (_,[a]) = maybe 0 (fromIntegral . view acctBalance) <$> addy a >>= push >> next
dispatch ORIGIN _ = reader origin >>= push >> next
dispatch CALLER _ = reader caller >>= push >> next
dispatch CALLVALUE _ = reader callValue >>= push >> next
dispatch CALLDATALOAD (_,[a]) = fromMaybe 0 . (V.!? int a) <$> reader callData >>=
                                push >> next
dispatch CALLDATASIZE _ = fromIntegral . V.length <$> reader callData >>= push >> next
dispatch CALLDATACOPY (_,[a,b,c]) = reader callData >>=
                                    mstores a (int b) (int c) >> next
dispatch CODESIZE _ = fromIntegral . V.length . pCode <$> reader prog >>= push >> next
dispatch CODECOPY (_,[a,b,c]) = bcsToWord8s . V.toList . pCode <$> reader prog >>=
                                codeCopy a (int b) (int c) >> next
dispatch GASPRICE _ = reader gasPrice >>= push >> next
dispatch EXTCODESIZE (_,[a]) = maybe 0 (fromIntegral . length . view acctCode) <$> addy a >>=
                               push >> next
dispatch EXTCODECOPY (_,[a,b,c,d]) = maybe (bcsToWord8s $ toByteCode STOP) (view acctCode) <$> addy a >>=
                                     codeCopy b (int c) (int d) >> next
dispatch BLOCKHASH _ = err "TODO"
dispatch COINBASE _ = reader coinbase >>= push >> next
dispatch TIMESTAMP _ = reader timestamp >>= push >> next
dispatch NUMBER _ = reader number >>= push >> next
dispatch DIFFICULTY _ = reader difficulty >>= push >> next
dispatch GASLIMIT _ = fromIntegral <$> reader gaslimit >>= push >> next
dispatch POP _ = next -- exec already pops 1 per spec
dispatch MLOAD (_,[a]) = mload a >>= push >> next
dispatch MSTORE (_,[a,b]) = mstore a b >> next
dispatch MSTORE8 (_,[a,b]) = mstore a b >> next
dispatch SLOAD (_,[a]) = sload a >>= push >> next
dispatch SSTORE (_,[a,b]) = sstore a b >> next
dispatch JUMP (_,[a]) = jump a
dispatch JUMPI (_,[a,b]) = if b /= 0 then jump a else next
dispatch PC _ = fromIntegral <$> use ctr >>= push >> next
dispatch MSIZE _ = fromIntegral . (* 8) . M.size <$> use mem >>= push >> next
dispatch GAS _ = fromIntegral <$> use gas >>= push >> next
dispatch JUMPDEST _ = next -- per spec: "Mark a valid destination for jumps."
                           -- "This operation has no effect on machine state during execution."
dispatch _ (Dup n,_) = dup n >> next
dispatch _ (Swap n,_) = swap n >> next
dispatch _ (Log n,_) = log n >> next
dispatch CREATE (_,[a,b,c]) = create (fromIntegral a) b (fromIntegral c)
dispatch CALL (_,[g,t,gl,io,il,oo,ol]) = doCall (fromIntegral g) t t (fromIntegral gl) io il oo (int ol)
dispatch CALLCODE (_,[g,t,gl,io,il,oo,ol]) = reader address >>= \a ->
                                             doCall (fromIntegral g) a t (fromIntegral gl) io il oo (int ol)
dispatch RETURN (_,[a,b]) = Return . concatMap u256ToW8s <$> mloads (a `div` 32) (b `div` 32)
dispatch SUICIDE (_,[a]) = suicide a
dispatch _ ps = err $ "Unsupported operation [" ++ show ps ++ "]"


----
-- TESTING
----

run_ :: String -> IO (Either String (Output TestExtData))
run_ = runBC_ . parseHex

runBC_ :: ToByteCode a => [a] -> IO (Either String (Output TestExtData))
runBC_ bc = runVM (emptyState ex gas')
            (Env dbug enableGas calldata testExt
             (toProg tbc)
             (_acctAddress acc)
             addr
             addr
             0 0 0 0 0 0 0 0 0)
            Nothing
    where tbc = concatMap toByteCode bc
          addr = 123456
          enableGas = True
          gas' = 10000000
          acc = ExtAccount (bcsToWord8s tbc) 0 addr M.empty
          ex = TestExtData (M.fromList [(addr,acc)]) S.empty S.empty 0
          calldata = V.fromList [0,1,2,3,4]
          dbug = True
          testExt :: Ext TestExtData
          testExt = Ext {
                      xStore = \a k v -> xover (edAccts . ix a . acctStore) (M.insert k v)
                    , xLoad = \a k -> xfirstOf (edAccts . ix a . acctStore . ix k)
                    , xAddress = \k -> xfirstOf (edAccts . ix k)
                    , xCreate = \g -> do
                                  newaddy <- succ . maximum . M.keys <$> xview edAccts
                                  let newacct = ExtAccount [] g newaddy M.empty
                                  xover edCreates (S.insert newaddy)
                                  xover edAccts (M.insert newaddy newacct)
                                  return newacct
                    , xSaveCode = \a ws -> setExt $ set (edAccts . ix a . acctCode) ws
                    , xSuicide = \a -> do
                                  justDeleted <- S.member a <$> xview edSuicides
                                  xover edSuicides (S.delete a)
                                  return justDeleted
                    , xRefund = \a -> xover edRefund (+ a)
                    , xIsCreate = \a -> S.member a <$> xview edCreates
                    }



data TestExtData = TestExtData {
      _edAccts :: M.Map U256 ExtAccount
    , _edSuicides :: S.Set U256
    , _edCreates :: S.Set U256
    , _edRefund :: Gas
} deriving (Eq,Show)

edAccts :: Lens' TestExtData (M.Map U256 ExtAccount)
edAccts f s = fmap (\a -> s { _edAccts = a }) (f $ _edAccts s)
edSuicides :: Lens' TestExtData (S.Set U256)
edSuicides f s = fmap (\a -> s { _edSuicides = a }) (f $ _edSuicides s)
edCreates :: Lens' TestExtData (S.Set U256)
edCreates f s = fmap (\a -> s { _edCreates = a }) (f $ _edCreates s)
edRefund :: Lens' TestExtData Gas
edRefund f s = fmap (\a -> s { _edRefund = a }) (f $ _edRefund s)
