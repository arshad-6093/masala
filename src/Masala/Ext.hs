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


newtype ExtOp e a = ExtOp { runExtOp :: e -> (a, e) } deriving (Functor)
instance Applicative (ExtOp e) where
    pure a = ExtOp (a,)
    (ExtOp f) <*> (ExtOp a) = ExtOp $ \e -> ((fst $ f e) (fst $ a e),e)
instance Monad (ExtOp e) where
    return = pure
    a >>= f = ExtOp $ \e -> (\(a',e') -> runExtOp (f a') e')  $ runExtOp a e


class HasExtState e a | a -> e where
    ext :: Lens' a e

class HasExtApi e a | a -> e where
    extApi :: Lens' a (Ext e)



toAddress :: Integral i => i -> Address
toAddress u = fromIntegral (u `mod` (2 ^ (160 :: Int)))


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

xover :: ASetter s s a b -> (a -> b) -> ExtOp s ()
xover l f = setExt $ over l f

xfirstOf :: Getting (Leftmost a) s a -> ExtOp s (Maybe a)
xfirstOf l = useExt $ firstOf l

xview :: Getting b s b -> ExtOp s b
xview l = view l <$> getExt

infixl 4 <@$>
(<@$>) :: (HasExtApi e r, MonadReader r m) => (Ext e -> x -> b) -> x -> m b
acc <@$> a = do f <- acc <$> view extApi; return $ f a

infixl 4 <@*>
(<@*>) :: Monad m => m (y -> c) -> y -> m c
f <@*> b = f >>= \f' -> return $ f' b

xRun :: (HasExtState e s, MonadState s m) => m (ExtOp e r) -> m r
xRun v = do
  (r,e) <- runExtOp <$> v <*> use ext
  ext .= e
  return r

data LogEntry = LogEntry {
      logAddress :: Address
    , logBlock :: U256
    , logTopics :: [U256]
    , logData :: [U8]
} deriving (Eq,Show)

data Ext e = Ext {
      xStore :: Address -> U256 -> U256 -> ExtOp e ()
    , xLoad :: Address -> U256 -> ExtOp e (Maybe U256)
    , xAddress :: Address -> ExtOp e (Maybe ExtAccount)
    , xCreate :: Gas -> ExtOp e ExtAccount
    , xSaveCode :: Address -> [U8] -> ExtOp e ()
    , xSuicide :: Address -> ExtOp e Bool
    , xRefund :: Address -> Gas -> ExtOp e ()
    , xIsCreate :: Address -> ExtOp e Bool
    , xLog :: LogEntry -> ExtOp e ()
}
