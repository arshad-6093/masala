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


type Gas = Integer


type Stack = [U256]
type Mem = M.Map U256 U8
type Ctr = Int
data VMState = VMState {
      _stack :: Stack
    , _ctr :: Ctr
    , _mem :: Mem
    , _gas :: Gas
} deriving (Eq,Show)



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



class (Monad m) => MonadExt m where
    extStore :: Address -> U256 -> U256 -> m ()
    extLoad :: Address -> U256 -> m (Maybe U256)
    extOut :: String -> m ()
    extDebug :: String -> m ()
    extAddress :: Address -> m (Maybe ExtAccount)
    extCreate :: Gas -> m ExtAccount
    extSaveCode :: Address -> [U8] -> m ()
    extSuicide :: Address -> m Bool
    extRefund :: Address -> Gas -> m ()
    extIsCreate :: Address -> m Bool
    extLog :: LogEntry -> m ()





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



data ExtAccount = ExtAccount {
      _acctCode :: [U8]
    , _acctBalance :: Gas
    , _acctAddress :: Address
    , _acctStore :: M.Map U256 U256
    } deriving (Eq)


instance Show ExtAccount where
    show (ExtAccount c b a s) =
        "ExtAccount {code=" ++ sc c ++ ", bal=" ++ show b ++ ", addy=" ++
                         show a ++ ", store=" ++ show s
            where sc [] = "[empty]"
                  sc _ = "*bytecode*"


toAddress :: Integral i => i -> Address
toAddress u = fromIntegral (u `mod` (2 ^ (160 :: Int)))


data LogEntry = LogEntry {
      logAddress :: Address
    , logBlock :: U256
    , logTopics :: [U256]
    , logData :: [U8]
} deriving (Eq,Show)


$(makeLenses ''VMState)
$(makeLenses ''Env)
$(makeLenses ''ExtAccount)


instance (MonadExt m) => MonadExt (VM m) where
    {-# INLINE extStore #-}
    {-# INLINE extLoad #-}
    {-# INLINE extOut #-}
    {-# INLINE extDebug #-}
    {-# INLINE extAddress #-}
    {-# INLINE extCreate #-}
    {-# INLINE extSaveCode #-}
    {-# INLINE extSuicide #-}
    {-# INLINE extRefund #-}
    {-# INLINE extIsCreate #-}
    {-# INLINE extLog #-}
    extStore a b c = lift $ extStore a b c
    extLoad a b = lift $ extLoad a b
    extOut a = lift $ extOut a
    extDebug a = lift $ extDebug a
    extAddress a = lift $ extAddress a
    extCreate a = lift $ extCreate a
    extSaveCode a b = lift $ extSaveCode a b
    extSuicide a = lift $ extSuicide a
    extRefund a b = lift $ extRefund a b
    extIsCreate a = lift $ extIsCreate a
    extLog a = lift $ extLog a

instance MonadExt m => MonadExt (ExceptT a m) where
    {-# INLINE extStore #-}
    {-# INLINE extLoad #-}
    {-# INLINE extOut #-}
    {-# INLINE extDebug #-}
    {-# INLINE extAddress #-}
    {-# INLINE extCreate #-}
    {-# INLINE extSaveCode #-}
    {-# INLINE extSuicide #-}
    {-# INLINE extRefund #-}
    {-# INLINE extIsCreate #-}
    {-# INLINE extLog #-}
    extStore a b c = lift $ extStore a b c
    extLoad a b = lift $ extLoad a b
    extOut a = lift $ extOut a
    extDebug a = lift $ extDebug a
    extAddress a = lift $ extAddress a
    extCreate a = lift $ extCreate a
    extSaveCode a b = lift $ extSaveCode a b
    extSuicide a = lift $ extSuicide a
    extRefund a b = lift $ extRefund a b
    extIsCreate a = lift $ extIsCreate a
    extLog a = lift $ extLog a

instance MonadExt m => MonadExt (StateT a m) where
    {-# INLINE extStore #-}
    {-# INLINE extLoad #-}
    {-# INLINE extOut #-}
    {-# INLINE extDebug #-}
    {-# INLINE extAddress #-}
    {-# INLINE extCreate #-}
    {-# INLINE extSaveCode #-}
    {-# INLINE extSuicide #-}
    {-# INLINE extRefund #-}
    {-# INLINE extIsCreate #-}
    {-# INLINE extLog #-}
    extStore a b c = lift $ extStore a b c
    extLoad a b = lift $ extLoad a b
    extOut a = lift $ extOut a
    extDebug a = lift $ extDebug a
    extAddress a = lift $ extAddress a
    extCreate a = lift $ extCreate a
    extSaveCode a b = lift $ extSaveCode a b
    extSuicide a = lift $ extSuicide a
    extRefund a b = lift $ extRefund a b
    extIsCreate a = lift $ extIsCreate a
    extLog a = lift $ extLog a


instance MonadExt m => MonadExt (ReaderT a m) where
    {-# INLINE extStore #-}
    {-# INLINE extLoad #-}
    {-# INLINE extOut #-}
    {-# INLINE extDebug #-}
    {-# INLINE extAddress #-}
    {-# INLINE extCreate #-}
    {-# INLINE extSaveCode #-}
    {-# INLINE extSuicide #-}
    {-# INLINE extRefund #-}
    {-# INLINE extIsCreate #-}
    {-# INLINE extLog #-}
    extStore a b c = lift $ extStore a b c
    extLoad a b = lift $ extLoad a b
    extOut a = lift $ extOut a
    extDebug a = lift $ extDebug a
    extAddress a = lift $ extAddress a
    extCreate a = lift $ extCreate a
    extSaveCode a b = lift $ extSaveCode a b
    extSuicide a = lift $ extSuicide a
    extRefund a b = lift $ extRefund a b
    extIsCreate a = lift $ extIsCreate a
    extLog a = lift $ extLog a
