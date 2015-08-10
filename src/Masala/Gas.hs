{-# LANGUAGE TupleSections #-}
module Masala.Gas where

import Masala.Instruction
import qualified Data.Map as M
import Prelude hiding (EQ,LT,GT)

type Gas = Int

gasZero :: [Instruction]
gasZero = [STOP, SUICIDE, RETURN]

gasBase :: [Instruction]
gasBase = [ADDRESS, ORIGIN, CALLER, CALLVALUE, CALLDATASIZE, CODESIZE, GASPRICE, COINBASE, TIMESTAMP, NUMBER, DIFFICULTY, GASLIMIT, POP, PC, MSIZE, GAS]

gasVeryLow :: [Instruction]
gasVeryLow = [ADD, SUB, NOT, LT, GT, SLT, SGT, EQ, ISZERO, AND, OR, XOR, BYTE, CALLDATALOAD, MLOAD, MSTORE, MSTORE8] ++ [PUSH1 .. PUSH32] ++ [DUP1 .. DUP16] ++ [SWAP1 .. SWAP16]

gasLow :: [Instruction]
gasLow = [MUL, DIV, SDIV, MOD, SMOD, SIGNEXTEND]

gasMid :: [Instruction]
gasMid = [ADDMOD, MULMOD, JUMP]

gasHigh :: [Instruction]
gasHigh = [JUMPI]

gasExt :: [Instruction]
gasExt = [BALANCE, EXTCODESIZE, BLOCKHASH]

fixedGas :: M.Map Instruction Gas
fixedGas = M.fromList $
          map (,0) gasZero ++
          map (,2) gasBase ++
          map (,3) gasVeryLow ++
          map (,5) gasLow ++
          map (,10) gasHigh ++
          map (,20) gasExt ++ 
          [(SLOAD,50)
          ,(JUMPDEST,1)
          ,(SUICIDE,-24000)]
           
                                  
          
{-

$G_{sset}$ & 20000 & Paid for an {\small SSTORE} operation when the storage value is set to non-zero from zero. \\
$G_{sreset}$ & 5000 & Paid for an {\small SSTORE} operation when the storage value's zeroness remains unchanged or is set to zero. \\
$R_{sclear}$ & 15000 & Refund given (added into refund counter) when the storage value is set to zero from non-zero. \\
$R_{suicide}$ & 24000 & Refund given (added into refund counter) for suiciding an account. \\
$G_{create}$ & 32000 & Paid for a {\small CREATE} operation. \\
$G_{codedeposit}$ & 200 & Paid per byte for a {\small CREATE} operation to succeed in placing code into state. \\
$G_{call}$ & 40 & Paid for a {\small CALL} operation. \\
$G_{callvalue}$ & 9000 & Paid for a non-zero value transfer as part of the {\small CALL} operation. \\
$G_{callstipend}$ & 2300 & A stipend for the called contract subtracted from $G_{callvalue}$ for a non-zero value transfer. \\
$G_{callnewaccount}$ & 25000 & Paid for a {\small CALL} operation to a not previously excisting account. \\
$G_{exp}$ & 10 & Partial payment for an {\small EXP} operation. \\
$G_{expbyte}$ & 10 & Partial payment when multiplied by $\lceil\log_{256}(exponent)\rceil$ for the {\small EXP} operation. \\
$G_{memory}$ & 3 & Paid for every additional word when expanding memory. \\
$G_{txdatazero}$ & 4 & Paid for every zero byte of data or code for a transaction. \\
$G_{txdatanonzero}$ & 68 & Paid for every non-zero byte of data or code for a transaction. \\
$G_{transaction}$ & 21000 & Paid for every transaction. \\
$G_{log}$ & 375 & Partial payment for a {\small LOG} operation. \\
$G_{logdata}$ & 8 & Paid for each byte in a {\small LOG} operation's data. \\
$G_{logtopic}$ & 375 & Paid for each topic of a {\small LOG} operation. \\
$G_{sha3}$ & 30 & Paid for each {\small SHA3} operation. \\
$G_{sha3word}$ & 6 & Paid for each word (rounded up) for input data to a {\small SHA3} operation. \\
$G_{copy}$ & 3 & Partial payment for {\small *COPY} operations, multiplied by words copied, rounded up. \\
-}
