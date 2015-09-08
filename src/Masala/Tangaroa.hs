{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Masala.Tangaroa where

import Masala.RPC
import Masala.Ext.Simple
import Data.IORef
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import GHC.Generics
import Masala.Ext
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Masala.VM.Types
import Masala.VM

data RPCCmd = RPCCmd { method :: String, params :: [Value] } deriving (Generic,Show)
instance FromJSON RPCCmd

initRPCState :: RPCState ExtData
initRPCState = RPCState
               (Env dbug enableGas calldata api (toProg []) (_acctAddress acc)
                addr
                addr
                0 0 0 0 0 0 0 0 0)
               ex
    where addr = 123456
          enableGas = True
          acc = ExtAccount [] 0 addr M.empty
          ex = ExtData (M.fromList [(addr,acc)]) S.empty S.empty M.empty []
          calldata = V.fromList [0,1,2,3,4]
          dbug = True


runEvmRPC :: IORef (RPCState ExtData) -> String -> IO String
runEvmRPC ior cmd = do
  ve :: Either String RPCCmd <- return $ eitherDecode (LBS.pack cmd)
  case ve of
    Left err -> return $ "runEvmRPC: invalid JSON: " ++ err
    Right (RPCCmd meth pms) -> do
            s <- readIORef ior
            (v,s') <- runRPCIO s meth pms
            writeIORef ior s'
            return (LBS.unpack $ encode v)
