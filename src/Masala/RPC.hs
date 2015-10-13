{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Masala.RPC where


import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A ((.=))
import Data.Aeson.Types (fieldLabelModifier,Parser)
import Control.Monad.Except
import Masala.Ext
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
import Control.Lens
import qualified Data.Vector as V

data RPCState e = RPCState { _rpcEnv :: Env e, _rpcExt :: e }
$(makeLenses ''RPCState)


type RPC e = ExceptT String (StateT (RPCState e) IO)


newtype WordArray = WordArray { getWords :: [Word8] }
    deriving (Eq,Generic)
instance FromJSON WordArray where
    parseJSON = withText "WordArray"
                (\t -> case readHexs (drop 2 $ T.unpack t) of
                         Right a -> return (WordArray (dropWhile (==0) a))
                         Left err -> fail err)
instance Show WordArray where show (WordArray a) = "0x" ++ concatMap showHex a


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

runRPC :: Show e => String -> [Value] -> RPC e Value
runRPC c v = do
  rpc <- maybe (throwError $ "Invalid RPC: " ++ c) return $ HM.lookup c rpcs
  dispatchRPC rpc v

runRPCIO :: Show e => RPCState e -> String -> [Value] -> IO (Value,RPCState e)
runRPCIO s c v = do
  (r,s') <- runStateT (runExceptT (runRPC c v)) s
  case r of
    Left err -> return (object ["error" A..= err], s')
    Right o -> return (o,s')


dispatchRPC :: Show e => RPCall -> [Value] -> RPC e Value
dispatchRPC Eth_sendTransaction [a] = arg a >>= sendTransaction
dispatchRPC Eth_call [a,b] = arg2 a b >>= uncurry ethCall
dispatchRPC m a = throwError $ "Unsupported RPC: " ++ show m ++ ", " ++ show a

arg :: (FromJSON a) => Value -> RPC e a
arg v = case fromJSON v of
          Error err -> throwError $ "JSON parse failure: " ++ err
          Success a -> return a

arg2 :: (FromJSON a, FromJSON b) => Value -> Value -> RPC e (a,b)
arg2 a b = (,) <$> arg a <*> arg b



ethCall :: Show e => EthCall -> U256 -> RPC e Value
ethCall m@(EthCall fromA toA callgas gasPx callvalue sdata) _blockNo = do -- blockNo TODO
  liftIO $ putStrLn $ "ethCall: " ++ show m -- TODO handle as "debug"
  env <- use rpcEnv
  extdata <- use rpcExt
  let xapi = _envExtApi env
      acctm = flip evalExtOp extdata $ xAddress xapi toA
  case acctm of
    Nothing -> throwError $ "ethCall: Bad address: " ++ show m
    Just acct -> do
      o <- callVM (fromMaybe toA fromA) toA callgas gasPx callvalue (_acctCode acct) (maybe [] getWords sdata)
      liftIO $ putStrLn $ "call: Success, output=" ++ showHexs o
      return $ String $ T.pack $ showHexs o -- TODO need toJSON



sendTransaction :: Show e => SendTran -> RPC e Value
sendTransaction m@(SendTran fromA toA callgas gasPx callvalue sdata _nonce) = do
  liftIO $ putStrLn $ "sendTransaction: " ++ show m -- TODO handle as "debug"
  env <- use rpcEnv
  extdata <- use rpcExt
  let xapi = _envExtApi env
      ((addr,acode),extdata') = flip runExtOp extdata $
        case toA of
          Nothing -> do
            acct <- xCreate xapi 0
            let ad = _acctAddress acct
                c = maybe [] getWords sdata
            return (ad,c)
          Just t -> do
            acctm <- xAddress xapi t
            case acctm of
              Nothing -> error "Bad address" -- TODO, ExtOp should support MonadError
              Just acct -> return (_acctAddress acct,_acctCode acct)
  liftIO $ print ((addr,acode),extdata')
  rpcExt .= extdata'
  o <- callVM fromA addr callgas gasPx callvalue acode [] -- TODO, trans may also accept ABI
  when (isNothing toA) $ rpcExt %= execExtOp (xSaveCode xapi addr o)
  acct' <- evalExtOp (xAddress xapi addr) <$> use rpcExt
  liftIO $ putStrLn $ "sendTransaction: Success, addr=" ++ show addr ++ ", output=" ++ show o
  return $ String $ T.pack $ "Success, addr=" ++ show addr ++ ", acct=" ++ show acct'





callVM :: (Show e) => Address -> Address -> Maybe U256 -> Maybe U256 -> Maybe U256 -> [Word8] -> [Word8] -> RPC e [Word8]
callVM toA fromA callgas gasPx callvalue ccode cdata' = do
  env <- use rpcEnv
  extdata <- use rpcExt
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
      vmstate = emptyState extdata (fromIntegral cgas')
  liftIO $ putStrLn $ "callVM: " ++ show vmstate ++ ", " ++ show cgas'
  r <- liftIO $ runVM vmstate env' Nothing -- TODO, this should be asynchronous, returning "transaction hash"
  case r of
    Left s -> throwError $ "ERROR in callVM: " ++ s
    Right (vr, vs) -> case vr of
      Final o -> do
        liftIO $ putStrLn $ "call: Success, output=" ++ showHexs o
        rpcEnv .= env'
        rpcExt .= _vmext vs
        return o
