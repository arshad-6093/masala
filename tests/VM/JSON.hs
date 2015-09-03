{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module VM.JSON where

import qualified Data.Map.Strict as M
import Masala.Instruction
import Masala.Ext
import Masala.VM
import Masala.VM.Types
import Data.Word
import Data.Aeson hiding ((.=),Success)
import Data.Aeson.Types hiding (parse,Success)
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe
import Prelude hiding (words)
import qualified Data.Set as S
import Control.Lens
import Control.Exception

runFile :: FilePath -> IO [TestResult]
runFile f = do
  ts <- readVMTests f
  case ts of
    Left err -> return [Err f $ "Decode failed: " ++ err]
    Right tcs -> mapM (uncurry (runTest False)) . M.toList $ tcs

readVMTests :: FilePath -> IO (Either String (M.Map String VMTest))
readVMTests f = eitherDecode <$> LBS.readFile f

runOne :: FilePath -> String -> IO TestResult
runOne f t = do
  ts <- readVMTests f
  case ts of
    Left err -> return $ Err t $ "Decode failed: " ++ err
    Right m -> case M.lookup t m of
                 Nothing -> return $ Err t $ "Unknown test, file " ++ f
                 Just tc -> do
                   r <- runTest True t tc
                   case r of
                     Failure{} -> putStrLn $ "Failure, testcase: " ++ show tc
                     _ -> return ()
                   return r


runTest :: Bool -> String -> VMTest -> IO TestResult
runTest dbg t tc = do
  putStrLn "-----------------"
  putStrLn t
  putStrLn "-----------------"
  let catcher :: SomeException -> IO (Either String (Output TestExtData))
      catcher e = return $ Left $ "Runtime exception: " ++ show e
  r <- catch (runVMTest dbg t tc) catcher
  case r of
    Left e -> return $ Err t $ "Runtime failure: " ++ e
    Right o -> do
           let tr = validateRun t tc o
           print tr
           return tr

data TestResult =
          Success String
        | Failure String VMTest (Output TestExtData) String
        | Err String String

instance Show TestResult where
    show (Success n) = "\nSUCCESS: " ++ n
    show (Err n e) = "\nERROR: " ++ n ++ ": " ++ e
    show (Failure n t o e) = "\nFAILURE: " ++ n ++ ": " ++ e

validateRun :: String -> VMTest -> Output TestExtData -> TestResult
validateRun n t o = either (Failure n t o) (const (Success n)) check
    where check = checkPost (vpost t) >> checkOutput (vout t)
          checkPost Nothing = Right ()
          checkPost (Just ts) =
              assertEqual "post accts match"
                              (M.mapWithKey toEacct ts)
                              (_edAccts . _vmext . snd $ o)
          checkOutput Nothing = Right ()
          checkOutput (Just ws) =
              case fst o of
                Final os -> assertEqual "output matches" (words ws) os
                r -> Left $ "FAILED: non-final result expected " ++ show ws ++ ", result: " ++ show r

assertEqual :: (Eq a, Show a) => String -> a -> a -> Either String ()
assertEqual msg a b | a == b = return ()
                 | otherwise = Left $ "FAILED: " ++ msg ++ ", intended=" ++
                               show a ++ ", actual=" ++ show b

runVMTest :: Bool -> String -> VMTest -> IO (Either String (Output TestExtData))
runVMTest dbg tname test = either (Left . (("Test failed: " ++ tname) ++)) Right <$>
                       runVM (emptyState exdata gas') env Nothing
    where env = Env {
               _debug = dbg
             , _doGas = True
             , _callData = V.fromList (w8sToU256s (words (edata ex)))
             , _envExtApi = testExt
             , _prog = toProg tbc
             , _address = eaddress ex
             , _origin = eorigin ex
             , _caller = ecaller ex
             , _envGas = fromMaybe 0 (vgas test)
             , _gasPrice = egasPrice ex
             , _callValue = evalue ex
             , _prevHash = previousHash tenv
             , _coinbase = currentCoinbase tenv
             , _timestamp = currentTimestamp tenv
             , _number = currentNumber tenv
             , _difficulty = currentDifficulty tenv
             , _gaslimit = maybe 0 fromIntegral $ currentGasLimit tenv
             }
          ex = vexec test
          tenv = venv test
          tbc = concatMap toByteCode (parse (words (ecode ex)))
          gas' = fromIntegral $ egas ex
          exdata = TestExtData (M.mapWithKey toEacct (vpre test))  S.empty S.empty M.empty []

          testExt :: Ext TestExtData
          testExt = Ext {
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
                                  xover edSuicides (S.delete a)
                                  return justDeleted
                    , xRefund = \a g -> xover edRefund (M.insertWith (+) a g)
                    , xIsCreate = \a -> S.member a <$> xview edCreates
                    , xLog = \l -> xover edLog (l:)
                    }

toEacct :: Address -> TestAcct -> ExtAccount
toEacct k acct = ExtAccount {
                   _acctCode = words (acode acct)
                 , _acctBalance = fromIntegral $ abalance acct
                 , _acctAddress = k
                 , _acctStore = astorage acct
                 }

type TestAccts = M.Map Address TestAcct

data VMTest = VMTest {
      vcallcreates :: Maybe [String] -- apparently unused in vmtests
    , venv :: TestEnv
    , vexec :: TestExec
    , vgas :: Maybe U256
    , vlogs :: Maybe [TestLog]
    , vout :: Maybe WordArray
    , vpost :: Maybe TestAccts
    , vpre :: TestAccts
    , vpostStateRoot :: Maybe String
} deriving (Eq,Show,Generic)
instance FromJSON VMTest where parseJSON = parsePrefixJSON 'v'



data TestEnv = TestEnv {
      currentCoinbase :: U256
    , currentDifficulty :: U256
    , currentGasLimit :: Maybe U256
    , currentNumber :: U256
    , currentTimestamp :: U256
    , previousHash :: U256
} deriving (Eq,Show,Generic)
instance FromJSON TestEnv

data TestExec = TestExec {
      eaddress :: Address
    , ecaller :: Address
    , ecode :: WordArray
    , edata :: WordArray
    , egas :: U256
    , egasPrice :: U256
    , eorigin :: Address
    , evalue :: U256
} deriving (Eq,Show,Generic)
instance FromJSON TestExec where parseJSON = parsePrefixJSON 'e'

data TestLog = TestLog {
      laddress :: Address
    , lbloom :: String
    , ldata :: String
    , topics :: [U256]
} deriving (Eq,Show,Generic)
instance FromJSON TestLog where parseJSON = parsePrefixJSON 'l'


data TestAcct = TestAcct {
      abalance :: U256
    , acode :: WordArray
    , anonce :: U256
    , astorage :: M.Map U256 U256
} deriving (Eq,Show,Generic)
instance FromJSON TestAcct where parseJSON = parsePrefixJSON 'a'

instance (FromJSON v) => FromJSON (M.Map U256 v) where
    parseJSON = parseMap (either error id . eitherReadHex)
instance (FromJSON v) => FromJSON (M.Map Address v) where
    parseJSON = parseMap (either error id . eitherReadHex)


newtype WordArray = WordArray { words :: [Word8] }
    deriving (Eq,Generic)
instance FromJSON WordArray where
    parseJSON = withText "WordArray"
                (\t -> case hexToWord8s (drop 2 $ T.unpack t) of
                         Right a -> return (WordArray a)
                         Left err -> fail err)
instance Show WordArray where show (WordArray a) = "0x" ++ concatMap showHex a


parsePrefixJSON :: (Generic a, GFromJSON (Rep a), FromJSON a) => Char -> Value -> Parser a
parsePrefixJSON p = genericParseJSON (defaultOptions { fieldLabelModifier = dropWhile (==p) })

parseMap :: (FromJSON (M.Map k' v), Ord k) =>
            (k' -> k) -> Value -> Parser (M.Map k v)
parseMap keyFun = fmap (hashMapKey keyFun) . parseJSON
    where hashMapKey kv = M.foldrWithKey (M.insert . kv) M.empty
