{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module JSONSpec where

import qualified Data.Map.Strict as M
import Masala.Word
import Masala.Instruction hiding (Spec,spec)
import Masala.Ext.Simple
import Masala.VM
import Masala.VM.Types
import Masala.RPC
import Data.Aeson hiding ((.=),Success)
import Data.Aeson.Types hiding (parse,Success)
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Vector as V
import Data.Maybe
import Prelude hiding (words)
import qualified Data.Set as S
import Control.Exception
import Control.Monad
import Test.Hspec
import System.Directory



data TestResult =
          Success { tname :: String }
        | Failure { tname :: String,
                    ttest :: VMTest,
                    tout :: (VMResult,VMState,ExtData),
                    terr :: String }
        | Err { tname :: String, terr :: String }
instance Show TestResult where
    show (Success n) = "SUCCESS: " ++ n
    show (Err n e) = "ERROR: " ++ n ++ ": " ++ e
    show (Failure n _t _o e) = "FAILURE: " ++ n ++ ": " ++ e


spec :: Spec
spec = do
  tfs <- runIO (filter ((".json" ==).reverse.take 5.reverse) <$> getDirectoryContents "testfiles")
  mapM_ (parallel.fileSpec) tfs


fileSpec :: FilePath -> Spec
fileSpec tf =
    describe tf $ do
      vts <- runIO $ readVMTests tf
      forM_ (M.toList vts) $ \(n,vt) ->
                  do
                    tr <- runIO $ runTest False n vt
                    let success = return () :: Expectation
                    case tr of
                      (Success {}) -> it n success
                      r -> case vskip vt of
                             Just reason -> it (n ++ " [UNSUPPORTED, " ++ reason ++ "]: " ++ show r) success
                             Nothing -> it n $ expectationFailure (show r)

-- | run test file with optional debug out
runFile :: Bool -> FilePath -> IO [TestResult]
runFile d f = readVMTests f >>= mapM (uncurry (runTest d)) . M.toList


-- | run one test with debug output
runOne :: FilePath -> String -> IO TestResult
runOne f t = do
  m <- readVMTests f
  case M.lookup t m of
    Nothing -> return $ Err t $ "Unknown test, file " ++ f
    Just tc -> do
               r <- runTest True t tc
               case r of
                 Failure{} -> putStrLn $ "Failure, testcase: " ++ show tc
                 _ -> return ()
               return r



-- | parse JSON
readVMTests :: FilePath -> IO (M.Map String VMTest)
readVMTests f = LBS.readFile ("testfiles/" ++ f) >>= either bad return . eitherDecode
    where bad err = throwIO $ userError $ "ERROR: decode failed: " ++ f ++ ": " ++ err

-- | execute VM test
runTest :: Bool -> String -> VMTest -> IO TestResult
runTest dbg t tc = do
  when dbg $ do
    putStrLn "-----------------"
    putStrLn t
    putStrLn "-----------------"
  let catcher :: SomeException -> IO (Either String (VMResult,VMState,ExtData))
      catcher e = return $ Left $ "Runtime exception: " ++ show e
  r <- catch (runVMTest dbg t tc) catcher
  case r of
    Left e -> if isNothing (vpost tc) && isNothing (vout tc)
              then return $ Success (t ++ " [with failure: " ++ e ++ "]")
              else return $ Err t $ "Runtime failure: " ++ e
    Right o -> do
           let tr = validateRun t tc o
           when dbg $ print tr
           return tr
{-
actionSuicides :: ExtData -> ExtData
actionSuicides ed = over edAccts (M.filterWithKey isSuicide) ed
    where isSuicide a _ = not $ a `S.member` (view edSuicides ed)
-}

validateRun :: String -> VMTest -> (VMResult,VMState,ExtData) -> TestResult
validateRun n t o@(vr,_vs,ed) = either (Failure n t o) (const (Success n)) check
    where check = checkPost (vpost t) >> checkOutput (vout t)
          checkPost Nothing = Right ()
          checkPost (Just ts) = assertPostAcctsMatch (M.mapWithKey toEacct (testAccts ts)) (_edAccts ed)
          checkOutput Nothing = Right ()
          checkOutput (Just ws) =
              case vr of
                Final os -> assertEqual "output matches" (getWords ws) os
                r -> Left $ "FAILED: non-final result expected " ++ show ws ++ ", result: " ++ show r

assertPostAcctsMatch :: M.Map Address ExtAccount -> M.Map Address ExtAccount -> Either String ()
assertPostAcctsMatch i a | i == a = return ()
                         | M.keys i /= M.keys a = assertEqual "post accts match" i a
                         | otherwise = either Left (const $ Right ()) $
                                       mapM testEach (M.toList $ M.intersectionWith (,) i a)

testEach :: (Address, (ExtAccount, ExtAccount)) -> Either String ()
testEach (k, (i, a)) = do
  let msg m = "Account '" ++ show k ++ "' " ++ m ++ " equal"
  assertEqual (msg "code") (_acctCode i) (_acctCode a)
  assertEqual (msg "balance") (_acctBalance i) (_acctBalance a)
  assertEqual (msg "store") (_acctStore i) (_acctStore a)

assertEqual :: (Eq a, Show a) => String -> a -> a -> Either String ()
assertEqual msg a b | a == b = return ()
                 | otherwise = Left $ "FAILED: " ++ msg ++ ", intended=" ++
                               show a ++ ", actual=" ++ show b

runVMTest :: Bool -> String -> VMTest -> IO (Either String (VMResult, VMState, ExtData))
runVMTest dbg testname test = do
  when dbg $ do
    putStrLn ("Test: " ++ show test)
    putStrLn ("Prog: " ++ show tbc)
  if null tbc then return $ Right (Final [],vmstate,exdata)
  else fixup <$> runMExt (launchVM vmstate env Nothing) exdata
    where vmstate = emptyState gas'
          fixup ((Left err,_),_) = Left $ "Test failed: " ++ testname ++ ": " ++ err
          fixup ((Right r,vs),e) = Right (r,vs,e)
          env = Env {
               _gasModel = EthGasModel
             , _callData = V.fromList (getWords (edata ex))
             , _prog = toProg tbc
             , _address = eaddress ex
             , _origin = eorigin ex
             , _caller = ecaller ex
             , _envGas = fromMaybe 0 (vgas test)
             , _gasPrice = egasPrice ex
             , _callValue = evalue ex
             , _prevHash = fromMaybe 0 $ previousHash tenv
             , _coinbase = currentCoinbase tenv
             , _timestamp = currentTimestamp tenv
             , _number = currentNumber tenv
             , _difficulty = currentDifficulty tenv
             , _gaslimit = maybe 0 fromIntegral $ currentGasLimit tenv
             }
          ex = vexec test
          tenv = venv test
          tbc = concatMap toByteCode (parse (getWords (ecode ex)))
          gas' = fromIntegral $ egas ex
          exdata = ExtData (M.mapWithKey toEacct (testAccts (vpre test)))  S.empty S.empty M.empty [] dbg


toEacct :: Address -> TestAcct -> ExtAccount
toEacct k acct = ExtAccount {
                   _acctCode = getWords (acode acct)
                 , _acctBalance = fromIntegral $ abalance acct
                 , _acctAddress = k
                 , _acctStore = testStore $ astorage acct
                 }

newtype TestAccts = TestAccts { testAccts :: M.Map Address TestAcct } deriving (Eq,Show)

data VMTest = VMTest {
      vexec :: TestExec
    , vskip :: Maybe String
    , vgas :: Maybe U256
    , vlogs :: Maybe [TestLog]
    , vout :: Maybe WordArray
    , vpost :: Maybe TestAccts
    , vpre :: TestAccts
    , vpostStateRoot :: Maybe String
    , venv :: TestEnv
    , vcallcreates :: Maybe [CallCreate] -- apparently unused in vmtests
} deriving (Eq,Show,Generic)
instance FromJSON VMTest where parseJSON = parseDropPfxJSON 1

data CallCreate = CallCreate {
      ccdata :: WordArray
    --, ccdestination :: Maybe U256 TODO empty strings?????
    , ccgasLimit :: U256
    , ccvalue :: U256
} deriving (Eq,Show,Generic)
instance FromJSON CallCreate where parseJSON = parseDropPfxJSON 2

data TestEnv = TestEnv {
      currentCoinbase :: U256
    , currentDifficulty :: U256
    , currentGasLimit :: Maybe U256
    , currentNumber :: U256
    , currentTimestamp :: U256
    , previousHash :: Maybe U256
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
instance FromJSON TestExec where parseJSON = parseDropPfxJSON 1

data TestLog = TestLog {
      laddress :: Address
    , lbloom :: String
    , ldata :: String
    , topics :: [U256]
} deriving (Eq,Show,Generic)
instance FromJSON TestLog where parseJSON = parseDropPfxJSON 1


newtype TestStore = TestStore { testStore :: M.Map U256 U256 } deriving (Eq,Show)


data TestAcct = TestAcct {
      abalance :: U256
    , acode :: WordArray
    , anonce :: U256
    , astorage :: TestStore
} deriving (Eq,Show,Generic)
instance FromJSON TestAcct where parseJSON = parseDropPfxJSON 1

instance FromJSON TestAccts where
    parseJSON = fmap TestAccts . parseMap (either error id . readHex)
instance FromJSON TestStore where
    parseJSON = fmap TestStore . parseMap (either error id . readHex)

parseMap :: (FromJSON (M.Map k' v), Ord k) =>
            (k' -> k) -> Value -> Parser (M.Map k v)
parseMap keyFun = fmap (hashMapKey keyFun) . parseJSON
    where hashMapKey kv = M.foldrWithKey (M.insert . kv) M.empty
