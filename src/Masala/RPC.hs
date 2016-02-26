{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Supports a subset of Eth RPC, namely eth_call and eth_sendTransaction
module Masala.RPC where


import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A ((.=))
import Data.Aeson.Types (fieldLabelModifier,Parser)
import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.HashMap.Strict as HM
import GHC.Generics
import Masala.Word
import Masala.Instruction
import Masala.VM.Types
import Masala.VM
import Control.Monad.State.Strict
import Data.Maybe
import qualified Data.Vector as V
import Masala.Ext.Simple

-- | Monad maintains RPC state as the environment for the VM,
-- wrapping the 'MonadExt' implementation.
type RPC = ExceptT String (StateT Env MExt)


-- | Nice type for RPC, should be in VM.Types most likely.
newtype WordArray = WordArray { getWords :: [U8] }
    deriving (Eq,Generic)
instance FromJSON WordArray where
    parseJSON = withText "WordArray"
                (\t -> case readHexs (drop 2 $ T.unpack t) of
                         Right a -> return (WordArray a)
                         Left err -> fail err)
instance Show WordArray where show (WordArray a) = "0x" ++ concatMap showHexPad a


parseDropPfxJSON :: (Generic a, GFromJSON (Rep a), FromJSON a) => Int -> Value -> Parser a
parseDropPfxJSON n = genericParseJSON (defaultOptions { fieldLabelModifier = drop n })


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



data SendTran = SendTran {
    stfrom :: Address, -- The address the transaction is send from.
    stto :: Maybe Address, -- (optional when creating new contract) The address the transaction is directed to.
    stgas :: Maybe U256, -- (optional, default: 90000) Integer of the gas provided for the transaction execution. It will return unused gas.
    stgasPrice :: Maybe U256, -- (optional, default: To-Be-Determined) Integer of the gasPrice used for each paid gas
    stvalue :: Maybe U256, -- (optional) Integer of the value send with this transaction
    stdata :: Maybe WordArray, -- (optional) The compiled code of a contract
    stnonce :: Maybe U256 -- (optional) Integer of a nonce. This allows to overwrite your own pending transactions that use the same nonce.
    } deriving (Generic,Show)

instance FromJSON SendTran where parseJSON = parseDropPfxJSON 2

data EthCall = EthCall {
  cfrom :: Maybe Address, -- (optional) The address the transaction is send from.
  cto :: Address, -- The address the transaction is directed to.
  cgas :: Maybe U256, -- (optional) Integer of the gas provided for the transaction execution. eth_call consumes zero gas, but this parameter may be needed by some executions.
  cgasPrice :: Maybe U256, --  (optional) Integer of the gasPrice used for each paid gas
  cvalue :: Maybe U256, -- (optional) Integer of the value send with this transaction
  cdata :: Maybe WordArray -- (optional) The compiled code of a contract
  -- cblockno :: U256 -- integer block number, or the string "latest", "earliest" or "pending", see the default block parameter TODO
  } deriving (Generic,Show)

instance FromJSON EthCall where parseJSON = parseDropPfxJSON 1

rpcs :: HM.HashMap String RPCall
rpcs = foldl (\m r -> HM.insert (lc1 (show r)) r m) HM.empty [minBound..maxBound]
    where lc1 (c:cs) = C.toLower c:cs
          lc1 _ = error "rpcs: bug"

-- | run RPC monad.
runRPC :: String -> [Value] -> RPC Value
runRPC c v = do
  rpc <- maybe (throwError $ "Invalid RPC: " ++ c) return $ HM.lookup c rpcs
  dispatchRPC rpc v

-- | Fire up backend, run RPC in it.
runRPCIO :: ExtData -> Env -> String -> [Value] -> IO (Value,Env,ExtData)
runRPCIO e s c v = do
  (r,e') <- runMExt (runStateT (runExceptT (runRPC c v)) s) e
  case r of
    (Left err,s') -> return (object ["error" A..= err], s',e')
    (Right o,s') -> return (o,s',e')


-- | Dispatch.
dispatchRPC :: RPCall -> [Value] -> RPC Value
dispatchRPC Eth_sendTransaction [a] = arg a >>= sendTransaction
dispatchRPC Eth_call [a,b] = arg2 a b >>= uncurry ethCall
dispatchRPC m a = throwError $ "Unsupported RPC: " ++ show m ++ ", " ++ show a

arg :: (FromJSON a) => Value -> RPC a
arg v = case fromJSON v of
          Error err -> throwError $ "JSON parse failure: " ++ err
          Success a -> return a

arg2 :: (FromJSON a, FromJSON b) => Value -> Value -> RPC (a,b)
arg2 a b = (,) <$> arg a <*> arg b



ethCall :: EthCall -> U256 -> RPC Value
ethCall m@(EthCall fromA toA callgas gasPx callvalue sdata) _blockNo = do -- blockNo TODO
  liftIO $ putStrLn $ "ethCall: " ++ show m -- TODO handle as "debug"
  acctm <- extAddress toA
  case acctm of
    Nothing -> throwError $ "ethCall: Bad address: " ++ show m
    Just acct -> do
      o <- callVM (fromMaybe toA fromA) toA callgas gasPx callvalue (_acctCode acct) (maybe [] getWords sdata)
      liftIO $ putStrLn $ "call: Success, output=" ++ showHexs o
      return $ String $ T.pack $ showHexs o -- TODO need toJSON



sendTransaction :: SendTran -> RPC Value
sendTransaction m@(SendTran fromA toA callgas gasPx callvalue sdata _nonce) = do
  liftIO $ putStrLn $ "sendTransaction: " ++ show m -- TODO handle as "debug"
  (addr,acode) <-
        case toA of
          Nothing -> do
            acct <- extCreate 0
            let ad = _acctAddress acct
                c = maybe [] getWords sdata
            return (ad,c)
          Just t -> do
            acctm <- extAddress t
            case acctm of
              Nothing -> error "Bad address" -- TODO, ExtOp should support MonadError
              Just acct -> return (_acctAddress acct,_acctCode acct)
  liftIO $ print (addr,acode)
  o <- callVM fromA addr callgas gasPx callvalue acode [] -- TODO, trans may also accept ABI
  when (isNothing toA) $ extSaveCode addr o
  acct' <- extAddress addr
  liftIO $ putStrLn $ "sendTransaction: Success, addr=" ++ show addr ++ ", output=" ++ show o
  return $ String $ T.pack $ "Success, addr=" ++ show addr ++ ", acct=" ++ show acct'




-- | Call into VM from RPC.
callVM :: Address -> Address -> Maybe U256 -> Maybe U256 -> Maybe U256 -> [U8] -> [U8] -> RPC [U8]
callVM toA fromA callgas gasPx callvalue ccode cdata' = do
  env <- get
  let cgas' = fromMaybe 90000 callgas
      env' = env {
        _caller = fromA,
        _origin = fromA,
        _address = toA,
        _envGas = cgas', -- TODO, not sure what env gas is ...
        _callValue = fromMaybe 0 callvalue,
        _gasPrice = fromMaybe 0 gasPx,
        _callData = V.fromList cdata',
        _prog = toProg (concatMap toByteCode . parse $ ccode)
        }
      vmstate = emptyState (fromIntegral cgas')
  liftIO $ putStrLn $ "callVM: " ++ show vmstate ++ ", " ++ show cgas'
  r <- launchVM vmstate env' Nothing -- TODO, this should be asynchronous, returning "transaction hash"
  case r of
    (Left s,_) -> throwError $ "ERROR in callVM: " ++ s
    (Right vr, _vs) -> case vr of
      Final o -> do
        liftIO $ putStrLn $ "call: Success, output=" ++ showHexs o
        put env'
        return o
      er -> error $ "callVM: result not final: " ++ show er
