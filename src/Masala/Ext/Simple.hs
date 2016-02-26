{-# LANGUAGE TemplateHaskell #-}

-- | Backend implementation which mutates an 'ExtData',
-- Runs in IO for debug logging.
module Masala.Ext.Simple where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Masala.VM.Types
import Masala.Word
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad

-- | Backend data structure.
data ExtData = ExtData {
      _edAccts :: M.Map Address ExtAccount
    , _edSuicides :: S.Set Address
    , _edCreates :: S.Set Address
    , _edRefund :: M.Map Address Gas
    , _edLog :: [LogEntry]
    , _edDebug :: Bool
} deriving (Eq,Show)

$(makeLenses ''ExtData)

emptyExtData :: ExtData
emptyExtData = ExtData mempty mempty mempty mempty mempty True

-- | MExt is State-like + IO
newtype MExt a = MExt { runMExt :: ExtData -> IO (a,ExtData) }

instance Functor MExt where
    fmap f (MExt a) = MExt $ \s -> do
                        (a',s') <- a s
                        return (f a',s')
instance Applicative MExt where
    pure a = MExt $ \s -> return (a,s)
    (MExt f) <*> (MExt a) = MExt $ \s -> do
                              (f',s') <- f s
                              (a',s'') <- a s'
                              return (f' a',s'')
instance Monad MExt where
    return = pure
    (MExt a) >>= f = MExt $ \s -> do
                       (a',s') <- a s
                       let (MExt b) = f a'
                       b s'

-- | State-like get.
extGet :: MExt ExtData
extGet = MExt $ \e -> return (e,e)

-- | State-like set.
extSet :: ExtData -> MExt ()
extSet d = MExt $ \_ -> return ((),d)

-- | State-like modify.
extModify :: (ExtData -> ExtData) -> MExt ()
extModify f = MExt $ \s -> return ((),f s)

-- | Lensy state modify.
extOver :: ASetter ExtData ExtData a b -> (a -> b) -> MExt ()
extOver l f = extModify $ over l f

-- | Lensy use.
extUse :: (ExtData -> a) -> MExt a
extUse f = fmap f extGet

-- | Traversal on state.
extFirstOf :: Getting (Leftmost a) ExtData a -> MExt (Maybe a)
extFirstOf l = extUse $ firstOf l

-- | Lensy read.
extView :: Getting b ExtData b -> MExt b
extView l = view l <$> extGet

-- | State-like exec.
extExec :: MExt a -> ExtData -> IO ExtData
extExec o = fmap snd . runMExt o

-- | State-like eval.
extEval :: MExt a -> ExtData -> IO a
extEval o = fmap fst . runMExt o


instance MonadExt MExt where

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
    extStore a k v = extOver (edAccts . ix a . acctStore)
                     (\m -> if v == 0 then M.delete k m else M.insert k v m)
    extLoad a k = extFirstOf (edAccts . ix a . acctStore . ix k)
    extAddress k = extFirstOf (edAccts . ix k)
    extCreate g = do
      newaddy <- succ . maximum . M.keys <$> extView edAccts
      let newacct = ExtAccount [] g newaddy M.empty
      extOver edCreates (S.insert newaddy)
      extOver edAccts (M.insert newaddy newacct)
      return newacct
    extSaveCode a ws = extModify $ set (edAccts . ix a . acctCode) ws
    extSuicide a = do
      justDeleted <- S.member a <$> extView edSuicides
      extOver edSuicides (S.insert a)
      return justDeleted
    extRefund a g = extOver edRefund (M.insertWith (+) a g)
    extIsCreate a = S.member a <$> extView edCreates
    extLog l = extOver edLog (l:)
    extOut = liftIO . putStrLn
    extDebug s = do
      d <- extView edDebug
      when d (liftIO $ putStrLn s)

instance MonadIO MExt where
    liftIO a = MExt $ \s -> do
                        a' <- a
                        return (a',s)
