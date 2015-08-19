{-# LANGUAGE TupleSections #-}
module Masala.Gas where

import Masala.Instruction
import qualified Data.Map as M
import Prelude hiding (EQ,LT,GT)
import Data.Maybe

type Gas = Int

data GasCalc =
    MemSize U256
        | StoreOp U256 U256 -- memloc, value
        | GasCall U256 (Maybe U256) -- memsize, calladdy
          deriving (Eq,Show)

-- TODO these differ from the go code but match the yellow paper ...

gasZero :: [Instruction]
gasZero = [STOP
          , SUICIDE
          , RETURN]

gasBase :: [Instruction]
gasBase = [ADDRESS
          , ORIGIN
          , CALLER
          , CALLVALUE
          , CALLDATASIZE
          , CODESIZE
          , GASPRICE
          , COINBASE
          , TIMESTAMP
          , NUMBER
          , DIFFICULTY
          , GASLIMIT
          , POP
          , PC
          , MSIZE
          , GAS]

gasVeryLow :: [Instruction]
gasVeryLow = [ADD
             , SUB
             , NOT
             , LT
             , GT
             , SLT
             , SGT
             , EQ
             , ISZERO
             , AND
             , OR
             , XOR
             , BYTE
             , CALLDATALOAD
             , MLOAD
             , MSTORE
             , MSTORE8]
             ++ [PUSH1 .. PUSH32]
             ++ [DUP1 .. DUP16]
             ++ [SWAP1 .. SWAP16]

gasLow :: [Instruction]
gasLow = [MUL
         , DIV
         , SDIV
         , MOD
         , SMOD
         , SIGNEXTEND]

gasMid :: [Instruction]
gasMid = [ADDMOD
         , MULMOD
         , JUMP]

gasHigh :: [Instruction]
gasHigh = [JUMPI]

gasExt :: [Instruction]
gasExt = [BALANCE
         , EXTCODESIZE
         , BLOCKHASH]

fixedGas :: M.Map Instruction Gas
fixedGas = M.fromList $
          map (,gas_zero) gasZero ++
          map (,gas_base) gasBase ++
          map (,gas_verylow) gasVeryLow ++
          map (,gas_low) gasLow ++
          map (,gas_high) gasHigh ++
          map (,gas_ext) gasExt ++
              [(SLOAD, gas_sload)
              ,(SHA3, gas_sha3)
              ,(CREATE,gas_create)
              ,(CALL,gas_call)
              ,(CALLCODE,gas_call)
              ,(JUMPDEST,gas_jumpdest)]


computeGas :: Instruction -> (ParamSpec,[U256]) -> (Gas,Maybe GasCalc)
computeGas i p = (\(g,c) -> (g + fgas,c)) $ iGas i p
                 where fgas = fromMaybe 0 $ M.lookup i fixedGas

memSize :: U256 -> U256 -> Maybe GasCalc
memSize a b = Just $ MemSize (a + b)

wordSize :: U256 -> Int
wordSize = length . u256ToW8s

callGas :: Instruction -> [U256] -> (Gas,Maybe GasCalc)
callGas i [g,t,gl,io,il,oo,ol] = (fromIntegral g + (if gl > 0 then gas_callvalue else 0),
                                  Just (GasCall (io + il + oo + ol)
                                                (if i == CALL then Just t else Nothing)))
callGas _ _ = (0,Nothing) -- error will be caught in dispatch

iGas :: Instruction -> (ParamSpec,[U256]) -> (Gas,Maybe GasCalc)
iGas _ (Log n,[a,b]) = (gas_log + (n * gas_logtopic) + (fromIntegral b * gas_logdata),
                       memSize a b)
iGas EXP (_,[_a,b]) = (gas_exp + (wordSize b * gas_expbyte), Nothing)
iGas SSTORE (_,[a,b]) = (0,Just $ StoreOp a b)
iGas SUICIDE _ = (0,Nothing) -- refund will happen in execution
iGas MLOAD (_,[a]) = (0,memSize a 32)
iGas MSTORE (_,[a,_]) = (0,memSize a 32)
iGas MSTORE8 (_,[a,_]) = (0,memSize a 1)
iGas RETURN (_,[a,b]) = (0,memSize a b)
iGas SHA3 (_,[a,b]) = (wordSize b * gas_sha3word,memSize a b)
iGas CALLDATACOPY (_,[a,_b,c]) = (wordSize c * gas_copy,memSize a c)
iGas CODECOPY (_,[a,_b,c]) = (wordSize c * gas_copy,memSize a c)
iGas EXTCODECOPY (_,[_a,b,_c,d]) = (wordSize d * gas_copy,memSize b d)
iGas CREATE (_,[_a,b,c]) = (0,memSize b c)
iGas CALL (_,s) = callGas CALL s
iGas CALLCODE (_,s) = callGas CALLCODE s
iGas _ _ = (0,Nothing)

gas_zero :: Gas; gas_zero = 0
gas_base :: Gas; gas_base = 2
gas_verylow :: Gas; gas_verylow = 3
gas_low :: Gas; gas_low = 5
gas_mid :: Gas; gas_mid = 8
gas_high :: Gas; gas_high = 10
gas_ext :: Gas; gas_ext = 20
gas_sload :: Gas; gas_sload = 50
gas_jumpdest :: Gas; gas_jumpdest = 1
gas_sset :: Gas; gas_sset = 20000
gas_sreset :: Gas; gas_sreset = 5000
gas_sclear :: Gas; gas_sclear = 15000
gas_suicide :: Gas; gas_suicide = 24000
gas_create :: Gas; gas_create = 32000
gas_codedeposit :: Gas; gas_codedeposit = 200
gas_call :: Gas; gas_call = 40
gas_callvalue :: Gas; gas_callvalue = 9000
gas_callstipend :: Gas; gas_callstipend = 2300
gas_callnewaccount :: Gas; gas_callnewaccount = 25000
gas_exp :: Gas; gas_exp = 10
gas_expbyte :: Gas; gas_expbyte = 10
gas_memory :: Gas; gas_memory = 3
gas_txdatazero :: Gas; gas_txdatazero = 4
gas_txdatanonzero :: Gas; gas_txdatanonzero = 68
gas_transaction :: Gas; gas_transaction = 21000
gas_log :: Gas; gas_log = 375
gas_logdata :: Gas; gas_logdata = 8
gas_logtopic :: Gas; gas_logtopic = 375
gas_sha3 :: Gas; gas_sha3 = 30
gas_sha3word :: Gas; gas_sha3word = 6
gas_copy :: Gas; gas_copy = 3
