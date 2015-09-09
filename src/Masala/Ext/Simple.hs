{-# LANGUAGE TemplateHaskell #-}
module Masala.Ext.Simple where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Masala.Ext
import Control.Lens

data ExtData = ExtData {
      _edAccts :: M.Map Address ExtAccount
    , _edSuicides :: S.Set Address
    , _edCreates :: S.Set Address
    , _edRefund :: M.Map Address Gas
    , _edLog :: [LogEntry]
} deriving (Eq,Show)

$(makeLenses ''ExtData)

api :: Ext ExtData
api = Ext {
        xStore = \a k v -> xover (edAccts . ix a . acctStore)
                 (\m -> if v == 0 then M.delete k m else M.insert k v m)
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
                     xover edSuicides (S.insert a)
                     return justDeleted
      , xRefund = \a g -> xover edRefund (M.insertWith (+) a g)
      , xIsCreate = \a -> S.member a <$> xview edCreates
      , xLog = \l -> xover edLog (l:)
      }


runApi_ :: ExtOp ExtData a -> (a, ExtData)
runApi_ f = runExtOp f (ExtData (M.fromList [(123456,ExtAccount [] 0 0 M.empty)]) S.empty S.empty M.empty [])
