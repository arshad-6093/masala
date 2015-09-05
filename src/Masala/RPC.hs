{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Masala.RPC where

import Data.Aeson
import Control.Monad.Except
import Control.Monad.IO.Class
import Masala.Ext
import qualified Data.Char as C
import qualified Data.HashMap.Strict as HM
import GHC.Generics

data RPCall =
        Eth_accounts |
        Eth_blockNumber |
        Eth_getBalance |
        Eth_protocolVersion |
        Eth_coinbase |
        Eth_mining |
        Eth_gasPrice |
        Eth_getStorage |
        Eth_storageAt |
        Eth_getStorageAt |
        Eth_getTransactionCount |
        Eth_getBlockTransactionCountByHash |
        Eth_getBlockTransactionCountByNumber |
        Eth_getUncleCountByBlockHash |
        Eth_getUncleCountByBlockNumber |
        Eth_getData |
        Eth_getCode |
        Eth_sign |
        Eth_sendRawTransaction |
        Eth_sendTransaction |
        Eth_transact |
        Eth_estimateGas |
        Eth_call |
        Eth_flush |
        Eth_getBlockByHash |
        Eth_getBlockByNumber |
        Eth_getTransactionByHash |
        Eth_getTransactionByBlockNumberAndIndex |
        Eth_getTransactionByBlockHashAndIndex |
        Eth_getUncleByBlockHashAndIndex |
        Eth_getUncleByBlockNumberAndIndex |
        Eth_getCompilers |
        Eth_compileSolidity |
        Eth_newFilter |
        Eth_newBlockFilter |
        Eth_newPendingTransactionFilter |
        Eth_uninstallFilter |
        Eth_getFilterChanges |
        Eth_getFilterLogs |
        Eth_getLogs |
        Eth_hashrate |
        Eth_getWork |
        Eth_submitWork |
        Eth_submitHashrate |
        Eth_resend |
        Eth_pendingTransactions |
        Eth_getTransactionReceipt
        deriving (Eq,Enum,Bounded,Show)

rpcs :: HM.HashMap String RPCall
rpcs = foldl (\m r -> HM.insert (lc1 (show r)) r m) HM.empty [minBound..maxBound]
    where lc1 (c:cs) = C.toLower c:cs

runRPC :: (MonadIO m, MonadError String m) => String -> Value -> m Value
runRPC c v = do
  rpc <- maybe (throwError $ "Invalid RPC: " ++ c) return $ HM.lookup c rpcs
  dispatchRPC rpc v

dispatchRPC :: (MonadIO m, MonadError String m) => RPCall -> Value -> m Value
dispatchRPC Eth_sendTransaction = call sendTransaction
dispatchRPC c = const (throwError $ "Unsupported RPC: " ++ show c)

call :: (MonadIO m, MonadError String m, FromJSON a) => (a -> m Value) -> Value -> m Value
call f v = f =<< case fromJSON v of
                   Error s -> throwError $ "Invalid RPC Payload: " ++
                              s ++ ": " ++ show v
                   Success a -> return a
                  
data Tran = Tran Address
          deriving (Generic)
instance FromJSON Tran where
                  
sendTransaction :: (MonadIO m, MonadError String m) => Tran -> m Value
sendTransaction t = undefined

         

