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
import Masala.Word
import qualified Data.Vector as V
import Prelude hiding (LT,GT,EQ,log)
import qualified Data.Map.Strict as M
import Masala.Ext.Simple
import qualified Data.Set as S
import Masala.VM.Types
import Masala.VM.Dispatch
import Masala.VM.Memory
import Masala.VM.Gas



toProg :: [ByteCode] -> Prog
toProg bc = Prog (V.fromList bc) (M.fromList (zipWith idx [0..] bc))
    where idx c (ByteCode n _ _) = (fromIntegral n,c)


forward :: Monad m => VM m Bool
forward = do
  c <- use ctr
  (Prog p _) <- view prog
  if c + 1 >= V.length p
  then return False
  else do
    ctr .= c + 1
    return True

emptyState :: Gas -> VMState
emptyState = VMState [] 0 M.empty

launchVM :: MonadExt m => VMState -> Env -> Maybe Resume -> m (Either String VMResult, VMState)
launchVM vm env callR = runVM vm env (stepVM callR) >>= postEx env


postEx :: MonadExt m => Env -> (Either String VMResult, VMState) -> m (Either String VMResult, VMState)
postEx _ l@(Left _,_) = return l
postEx _ r@(Right (Final {}),_) = return r
postEx env (Right (Call g addr codes _glimit cdata action), vm) = do
  let parsedcode = parse codes
  case parsedcode of
    Left e -> return (Left e,vm)
    Right prog' -> do
      let  env' = set prog (toProg prog') .
                 set address (_acctAddress addr) .
                 set caller (view address env) .
                 set callData (V.fromList cdata) $ env
      r <- launchVM (emptyState (fromIntegral g)) env' Nothing
      case r of
        (Left _,_) -> return r
        (Right (Call {}),v) -> return $ (Left $ "VM mrror: Call returned from 'runVM': " ++ show r,v)
        (Right (Final o),vm') ->
            launchVM vm' env (Just $ Resume 1 o action)


stepVM :: (MonadExt m) => Maybe Resume -> VM m VMResult
stepVM r = do
  let done ws = do
             get >>= \s -> extDebug $ show (ws,s)
             return (Final ws)
  cf <- case r of
          Nothing -> exec
          (Just rs@(Resume p result action)) -> do
              extDebug $ "Resume: " ++ show rs
              case action of
                SaveMem loc len -> mstores loc 0 len result
                SaveCode addr -> extSaveCode addr result
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
             extDebug $ "Yield: " ++ show call
             return call



exec :: MonadExt m => VM m ControlFlow
exec = do
  bc@(ByteCode _ i ws) <- current
  let (Spec _ stackin _ pspec) = spec i
  svals <- pops stackin
  debugOut bc svals
  gm <- view gasModel
  handleGas gm i pspec svals
  if null ws
  then dispatch i (pspec,svals)
  else mapM_ push (u8sToU256s ws) >> next


debugOut :: (MonadExt m) => ByteCode -> [U256] -> VM m ()
debugOut i svals = do
  vm <- get
  extDebug $ show (i,svals,vm)


----
-- TESTING
----

run_ :: String -> IO ((Either String VMResult, VMState),ExtData)
run_ = either error runBC_ . parseHex

runBC_ :: ToByteCode a => [a] -> IO ((Either String VMResult, VMState),ExtData)
runBC_ c = runVM_ c [0,1,2,3,4]

runHex :: String -> String -> IO ((Either String VMResult, VMState),ExtData)
runHex c d = runVM_ (either error id (parseHex c)) (either error id (readHexs d))

runVM_ :: ToByteCode a => [a] -> [U8] -> IO ((Either String VMResult, VMState),ExtData)
runVM_ bc calld = flip runMExt ex $ launchVM (emptyState gas')
            (Env EthGasModel calldata
             (toProg tbc)
             (_acctAddress acc)
             addr
             addr
             0 0 0 0 0 0 0 0 0)
            Nothing
    where tbc = concatMap toByteCode bc
          addr = 123456
          gas' = 10000000
          acc = ExtAccount (bcsToU8s tbc) 0 addr M.empty
          ex = ExtData (M.fromList [(addr,acc)]) S.empty S.empty M.empty [] True
          calldata = V.fromList calld
