{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Masala.VM.Types where

import Control.Lens
import Masala.Word
import Masala.Instruction
import Masala.Ext
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except


data Prog = Prog {
      -- parsed bytecode
      pCode :: V.Vector ByteCode
      -- map of valid codepoints to pCode indexes
    , pCodeMap :: M.Map U256 Int
} deriving (Eq, Show)

type Stack = [U256]
type Mem = M.Map U256 U8
type Ctr = Int
data VMState = VMState {
      _stack :: Stack
    , _ctr :: Ctr
    , _mem :: Mem
    , _gas :: Gas
} deriving (Eq,Show)

$(makeLenses ''VMState)


data CallAction = SaveMem U256 U256 | SaveCode Address deriving (Eq,Show)


data Resume = Resume {
      rPush :: U256,
      rResult ::[U8],
      rAction :: CallAction
    } deriving (Eq,Show)


data Env = Env {
      _debug :: Bool
    , _doGas :: Bool
    , _callData :: V.Vector U8
    , _prog :: Prog
    , _address :: Address
    , _origin :: Address
    , _caller :: Address
    , _envGas :: U256
    , _gasPrice :: U256
    , _callValue :: U256
    , _prevHash :: U256
    , _coinbase :: U256
    , _timestamp :: U256
    , _number :: U256
    , _difficulty :: U256
    , _gaslimit :: Gas
} deriving (Eq,Show)

$(makeLenses ''Env)


data VMResult =
    Final { fReturn :: [U8] } |
    Call {
      cGas :: Gas,
      cAcct :: ExtAccount,
      cCode :: [U8],
      cGasLimit :: Gas,
      cData :: [U8],
      cAction :: CallAction }
    deriving (Eq,Show)


newtype VM m a = VM { unVM :: ExceptT String (ReaderT Env (StateT VMState m)) a }
    deriving (Functor,Applicative,Monad,MonadError String,MonadReader Env,MonadState VMState)

instance (MonadExt m) => MonadExt (VM m) where
    extStore a b = lift . extStore a b
    extLoad a = lift . extLoad a
    extOut = lift . extOut
    extDebug = lift . extDebug
    extAddress = lift . extAddress
    extCreate = lift . extCreate
    extSaveCode a = lift . extSaveCode a
    extSuicide = lift . extSuicide
    extRefund a = lift . extRefund a
    extIsCreate = lift . extIsCreate
    extLog = lift . extLog

instance MonadTrans VM where
    lift g = VM $ ExceptT $ ReaderT $ \_ -> StateT $ \s -> fmap (\v -> (Right v,s)) $ g

{-# INLINE runVM #-}
runVM :: (Monad m) => VMState -> Env -> VM m a -> m (Either String a, VMState)
runVM vm env go = runStateT (runReaderT (runExceptT (unVM go)) env) vm

data ControlFlow =
          Next
        | Stop
        | Jump Int
        | Return [U8]
        | Yield VMResult
    deriving (Show)

emptyVMEnv :: Env
emptyVMEnv = Env False False mempty (Prog mempty mempty) 0 0 0 0 0 0 0 0 0 0 0 0
emptyVMState :: VMState
emptyVMState = VMState mempty 0 mempty 0
