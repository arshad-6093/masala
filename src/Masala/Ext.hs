{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Masala.Ext where

import Masala.Word
import Data.Aeson hiding ((.=))
import Control.Lens hiding (op)
import qualified Data.Map.Strict as M
import Control.Monad.Reader
import Control.Monad.State.Strict
import GHC.Generics

type Gas = Integer

newtype Address = Address U256 deriving (Num,Eq,Ord,Bounded,Enum,Integral,Real,Generic)
instance Show Address where show (Address u) = showHex u

instance FromJSON Address where parseJSON = parseJSONHex "Address"

data ExtAccount = ExtAccount {
      _acctCode :: [U8]
    , _acctBalance :: Gas
    , _acctAddress :: Address
    , _acctStore :: M.Map U256 U256
    } deriving (Eq)

$(makeLenses ''ExtAccount)

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
