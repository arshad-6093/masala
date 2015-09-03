{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Masala.Ext where

import Masala.Instruction
import Data.Aeson hiding ((.=))
import Control.Lens hiding (op)
import Data.Word
import qualified Data.Map.Strict as M
import Control.Monad.Reader
import Control.Monad.State
import GHC.Generics

type Gas = Int

newtype Address = Address U256 deriving (Num,Eq,Ord,Bounded,Enum,Integral,Real,Generic)
instance Show Address where show (Address u) = showHex u

instance FromJSON Address where parseJSON = parseJSONHex "Address"

data ExtAccount = ExtAccount {
      _acctCode :: [Word8]
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
    (ExtOp a) >>= f = ExtOp $ \e -> runExtOp (f (fst $ a e)) e

class HasExtState e a | e -> a where
    ext :: Lens' a e

class HasExtApi e a | e -> a where
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

xover :: Profunctor p => Setting p s s a b -> p a b -> ExtOp s ()
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
    , logData :: [U256]
} deriving (Eq,Show)

data Ext e = Ext {
      xStore :: Address -> U256 -> U256 -> ExtOp e ()
    , xLoad :: Address -> U256 -> ExtOp e (Maybe U256)
    , xAddress :: Address -> ExtOp e (Maybe ExtAccount)
    , xCreate :: Gas -> ExtOp e ExtAccount
    , xSaveCode :: Address -> [Word8] -> ExtOp e ()
    , xSuicide :: Address -> ExtOp e Bool
    , xRefund :: Address -> Gas -> ExtOp e ()
    , xIsCreate :: Address -> ExtOp e Bool
    , xLog :: LogEntry -> ExtOp e ()
}
