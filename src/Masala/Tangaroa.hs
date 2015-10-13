{-# LANGUAGE OverloadedStrings #-}
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
import Control.Exception
import qualified Data.Text as T
import Masala.Word
import Masala.VM.Dispatch (sha3)
import Data.Char
import Control.Monad

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
            (v,s') <- catch (runRPCIO s meth pms) (catchErr s)
            writeIORef ior s'
            return (LBS.unpack $ encode v)

catchErr :: RPCState ExtData -> SomeException -> IO (Value,RPCState ExtData)
catchErr s e = return (object ["error" .= T.pack ("Exception occured: " ++ show e)],s)

_runRPC :: String -> IO String
_runRPC s = do
  r <- newIORef initRPCState
  runEvmRPC r s

strToSha3 :: String -> U256
strToSha3 = sha3 . map (fromIntegral . ord)

abiZero :: U256 -> U256
abiZero a = a .&. (0xffffffff `shiftL` 224)

_repl :: IO ()
_repl = do
  r <- newIORef initRPCState
  forever $ do
          putStr "> "
          inp <- getLine
          o <- runEvmRPC r inp
          putStrLn o


abi :: String -> [U256] -> String
abi fn args = show (abiZero (strToSha3 fn)) ++ concatMap hex32 args
    where hex32 = reverse . take 8 . (++ cycle "0") . reverse . showHex
