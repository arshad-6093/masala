{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Masala.VM where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Lens hiding (op)
import Masala.Instruction
import Control.Monad.Except
import qualified Data.Vector as V
import Control.Applicative
import Prelude hiding (LT,GT,EQ,log)
import qualified Data.Map.Strict as M
import Masala.Gas
import Masala.Ext
import Masala.Ext.Simple
import qualified Data.Set as S
import Masala.VM.Types
import Masala.VM.Dispatch



toProg :: [ByteCode] -> Prog
toProg bc = Prog (V.fromList bc) (M.fromList (zipWith idx [0..] bc))
    where idx c (ByteCode n _ _) = (fromIntegral n,c)


forward :: VM m e => m Bool
forward = do
  c <- use ctr
  (Prog p _) <- view prog
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
postEx env (Right (Call g addr codes _glimit cdata action, vm)) = do
  let parsedcode = parse codes
  case parsedcode of
    Left e -> return $ Left e
    Right prog' -> do
      let es = view ext vm
          env' = set prog (toProg prog') .
                 set address (_acctAddress addr) .
                 set caller (view address env) .
                 set callData (V.fromList cdata) $ env
      r <- runVM (emptyState es (fromIntegral g)) env' Nothing
      case r of
        Left _ -> return r
        (Right (Call {},_)) -> return $ Left $ "VM error: Call returned from 'runVM': " ++ show r
        (Right (Final o,vm')) ->
            runVM vm env (Just $ Resume 1 o action (view ext vm'))


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
                SaveMem loc len -> mstores loc 0 len result
                SaveCode addr -> xRun $ xSaveCode <@$> addr <@*> result
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

computeMemGas :: VM m e => U256 -> m Gas
computeMemGas newSzBytes = do
  let toWordSize v = (v + 31) `div` 32
      newSzWords = fromIntegral $ toWordSize newSzBytes
      fee s = ((s * s) `div` 512) + (s * gas_memory)
  oldSzWords <- M.size <$> use mem
  return $ if newSzWords > oldSzWords
           then fee newSzWords - fee oldSzWords
           else 0

computeStoreGas :: VM m e => U256 -> U256 -> m Gas
computeStoreGas l v' = do
  v <- mload l
  if v == 0 && v' /= 0
  then return gas_sset
  else if v /= 0 && v' == 0
       then refund gas_sclear >> return gas_sreset
       else return gas_sreset


computeCallGas :: VM m e => Maybe Address -> m Gas
computeCallGas Nothing = return 0
computeCallGas (Just a) = do
  isNew <- xRun $ xIsCreate <@$> a
  return $ if isNew then gas_callnewaccount else 0


doDebug :: VM m e => m () -> m ()
doDebug a = do
  d <- view debug
  when d a

debugOut :: (Show e, VM m e) => ByteCode -> [U256] -> m ()
debugOut i svals = do
  vm <- get
  liftIO $ print (i,svals,vm)


----
-- TESTING
----

run_ :: String -> IO (Either String (Output ExtData))
run_ = either error runBC_ . parseHex

runBC_ :: ToByteCode a => [a] -> IO (Either String (Output ExtData))
runBC_ bc = runVM (emptyState ex gas')
            (Env dbug enableGas calldata api
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
          ex = ExtData (M.fromList [(addr,acc)]) S.empty S.empty M.empty []
          calldata = V.fromList [0,1,2,3,4]
          dbug = True
