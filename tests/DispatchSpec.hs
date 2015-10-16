module DispatchSpec where

import Masala.VM.Types
import Masala.VM.Dispatch
import Test.Hspec


testMemory :: Spec
testMemory = do
  it "mstore->mload" $ (runD $ mstore 0 20 >> mload 0) `shouldOutput` 20
  it "mstore8->mload" $ (runD $ mstore8 31 20 >> mload 0) `shouldOutput` 20

testMSize :: Spec
testMSize = do
  it "rounds up 32" $ (runD $ mstore 0x5a 0xeeee >> msize) `shouldOutput` 0x80

shouldOutput :: (Eq a,Show a) => IO (Either String a, VMState) -> a -> Expectation
shouldOutput action expected = do
  a <- action
  case a of
    (Left s,_) -> expectationFailure $ "Failure occured: " ++ show s
    (Right r,_) -> r `shouldBe` expected


runD :: VM IO a -> IO (Either String a, VMState)
runD act = runVM emptyVMState emptyVMEnv act
