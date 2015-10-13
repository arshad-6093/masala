{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Masala.VM.Types where

import Control.Lens
import Masala.Word
import Masala.Instruction
import Masala.Ext
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except


data Prog = Prog {
      -- parsed bytecode
      pCode :: V.Vector ByteCode
      -- map of valid codepoints to pCode indexes
    , pCodeMap :: M.Map U256 Int
}

type Stack = [U256]
type Mem = M.Map U256 Word8
type Ctr = Int
data VMState e = VMState {
      _stack :: Stack
    , _ctr :: Ctr
    , _mem :: Mem
    , _vmext :: e
    , _gas :: Gas
} deriving (Eq,Show)

$(makeLenses ''VMState)

instance HasExtState e (VMState e) where ext = vmext

data CallAction = SaveMem U256 U256 | SaveCode Address deriving (Eq,Show)


data Resume e = Resume {
      rPush :: U256,
      rResult ::[Word8],
      rAction :: CallAction,
      rExt :: e
    } deriving (Eq,Show)


data Env e = Env {
      _debug :: Bool
    , _doGas :: Bool
    , _callData :: V.Vector Word8
    , _envExtApi :: Ext e
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
}

$(makeLenses ''Env)

instance HasExtApi e (Env e) where extApi = envExtApi

data VMResult =
    Final { fReturn :: [Word8] }
        | Call {
            cGas :: Gas,
            cAcct :: ExtAccount,
            cCode :: [Word8],
            cGasLimit :: Gas,
            cData :: [Word8],
            cAction :: CallAction }
          deriving (Eq,Show)

type Output e = (VMResult, VMState e)

type VM m e = (Monad m
              ,MonadIO m
              ,MonadState (VMState e) m
              ,MonadError String m
              ,MonadReader (Env e) m
              )

unVM :: (MonadIO m, Functor m, Show ext) =>
         VMState ext -> Env ext ->
         ExceptT String (ReaderT (Env ext) (StateT (VMState ext) m)) a -> m (Either String a)
unVM vm env go = evalStateT (runReaderT (runExceptT go) env) vm

data ControlFlow e =
          Next
        | Stop
        | Jump Int
        | Return [Word8]
        | Yield VMResult
    deriving (Show)
