module DispatchSpec where

import Masala.VM.Types
import Masala.VM.Dispatch
import Masala.Instruction hiding (Spec)
import Masala.Word
import Masala.Ext
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Control.Monad.State.Strict hiding (state)
import Test.Hspec

type Result a = Either String (a,VMState Mocks)

testMemory :: Spec
testMemory = do
  it "mstore->mload" $ (runD $ mstore 0 20 >> mload 0) `shouldOutput` 20
  it "mstore8->mload" $ (runD $ mstore8 31 20 >> mload 0) `shouldOutput` 20

testMSize :: Spec
testMSize = do
  it "rounds up 32" $ (runD $ mstore 0x5a 0xeeee >> msize) `shouldOutput` 0x80

shouldOutput :: (Eq a,Show a) => IO (Result a) -> a -> Expectation
shouldOutput action expected = do
  a <- action
  case a of
    Left s -> expectationFailure $ "Failure occured: " ++ show s
    Right (r,_) -> r `shouldBe` expected


runD :: VM Mocks a -> IO (Result a)
runD act = unVM state env $ do
             a <- act
             s <- get
             return (a,s)

env :: Env Mocks
env = Env True False V.empty xapi (Prog V.empty M.empty) 0 0 0 0 0 0 0 0 0 0 0 0

state :: VMState Mocks
state = VMState [] 0 M.empty (Mock "default" mock) 0

data Mocks = Mocks String [Ext Mocks] |
             Mock String (Ext Mocks)

instance Show Mocks where
    show (Mocks s _) = s
    show (Mock s _) = s

instance Eq Mocks where _ == _ = True

mock :: Ext Mocks
mock = Ext {
        xStore = \_a _b _c -> return ()
      , xLoad = \_a _b -> return Nothing
      , xAddress = \_a -> return Nothing
      , xCreate = \_a -> return $ ExtAccount [] 0 0 M.empty
      , xSaveCode = \_a _b -> return ()
      , xSuicide = \_a -> return False
      , xRefund = \_a _b -> return ()
      , xIsCreate = \_a -> return False
      , xLog = \_a -> return ()
      }

xapi :: Ext Mocks
xapi = Ext {
        xStore = \a b c -> popMock >>= \f -> xStore f a b c
      , xLoad = \a b -> popMock >>= \f -> xLoad f a b
      , xAddress = \a -> popMock >>= \f -> xAddress f a
      , xCreate = \a -> popMock >>= \f -> xCreate f a
      , xSaveCode = \a b -> popMock >>= \f -> xSaveCode f a b
      , xSuicide = \a -> popMock >>= \f -> xSuicide f a
      , xRefund = \a b -> popMock >>= \f -> xRefund f a b
      , xIsCreate = \a -> popMock >>= \f -> xIsCreate f a
      , xLog = \a -> popMock >>= \f -> xLog f a
      }

popMock :: ExtOp Mocks (Ext Mocks)
popMock = do
  ms <- getExt
  case ms of
    Mock _ m -> return m
    Mocks _ [] -> error "Mocks empty"
    Mocks msg (m:s) -> do
            setExt $ const $ Mocks msg s
            return m
