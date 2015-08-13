{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Masala.Instruction where

import Data.Word

import Prelude hiding (LT,GT,EQ)
import qualified Data.Map as M
import Numeric
import Data.DoubleWord
import Data.Bits

 -- All instructions.
data Instruction =
      STOP
    | ADD
    | MUL
    | SUB
    | DIV
    | SDIV
    | MOD
    | SMOD
    | ADDMOD
    | MULMOD
    | EXP
    | SIGNEXTEND
    | LT
    | GT
    | SLT
    | SGT
    | EQ
    | ISZERO
    | AND
    | OR
    | XOR
    | NOT
    | BYTE
    | SHA3
    | ADDRESS
    | BALANCE
    | ORIGIN
    | CALLER
    | CALLVALUE
    | CALLDATALOAD
    | CALLDATASIZE
    | CALLDATACOPY
    | CODESIZE
    | CODECOPY
    | GASPRICE
    | EXTCODESIZE
    | EXTCODECOPY
    | BLOCKHASH
    | COINBASE
    | TIMESTAMP
    | NUMBER
    | DIFFICULTY
    | GASLIMIT
    | POP
    | MLOAD
    | MSTORE
    | MSTORE8
    | SLOAD
    | SSTORE
    | JUMP
    | JUMPI
    | PC
    | MSIZE
    | GAS
    | JUMPDEST
    | PUSH1
    | PUSH2
    | PUSH3
    | PUSH4
    | PUSH5
    | PUSH6
    | PUSH7
    | PUSH8
    | PUSH9
    | PUSH10
    | PUSH11
    | PUSH12
    | PUSH13
    | PUSH14
    | PUSH15
    | PUSH16
    | PUSH17
    | PUSH18
    | PUSH19
    | PUSH20
    | PUSH21
    | PUSH22
    | PUSH23
    | PUSH24
    | PUSH25
    | PUSH26
    | PUSH27
    | PUSH28
    | PUSH29
    | PUSH30
    | PUSH31
    | PUSH32
    | DUP1
    | DUP2
    | DUP3
    | DUP4
    | DUP5
    | DUP6
    | DUP7
    | DUP8
    | DUP9
    | DUP10
    | DUP11
    | DUP12
    | DUP13
    | DUP14
    | DUP15
    | DUP16
    | SWAP1
    | SWAP2
    | SWAP3
    | SWAP4
    | SWAP5
    | SWAP6
    | SWAP7
    | SWAP8
    | SWAP9
    | SWAP10
    | SWAP11
    | SWAP12
    | SWAP13
    | SWAP14
    | SWAP15
    | SWAP16
    | LOG0
    | LOG1
    | LOG2
    | LOG3
    | LOG4
    | CREATE
    | CALL
    | CALLCODE
    | RETURN
    | SUICIDE
  deriving (Eq,Show,Enum,Ord,Bounded)

-- | map Word8s to Instructions.
valueToInst :: M.Map Word8 Instruction
valueToInst = M.fromList $ map assn [minBound .. maxBound]
    where assn i = (value (spec i),i)

-- | convert hex string to Word8s.
hexToWord8s :: String -> [Word8]
hexToWord8s prog = conv 0 [] prog
    where conv :: Int -> [Word8] -> String -> [Word8]
          conv _ is [] = reverse is
          conv _ _is [_c] = error $ "Malformed program, single char at end: " ++ prog
          conv idx is (c1:c2:cs) =
              case readHex [c1,c2] of
                [(w,"")] -> conv (succ idx) (w:is) cs
                _ -> error $ "Invalid hex (index " ++
                     show idx ++ ", value " ++ [c1,c2] ++ ")"

-- | parse Word8s to bytecode rep.
parse :: [Word8] -> [ByteCode]
parse prog = inst [] . zip [0..] $ prog
    where inst bcs [] = reverse bcs
          inst bcs ((idx,v):ws) =
              case M.lookup v valueToInst of
                Nothing -> err idx "Instruction expected"
                Just i ->
                    case paramSpec (spec i) of
                      Push n -> push idx n (Inst i:bcs) ws
                      _ -> inst (Inst i:bcs) ws
          push idx n bcs ws | n > length ws =
                                err idx ("PUSH" ++show n ++ ": not enough input")
                            | otherwise =
                                inst (PushV (w8sToU256 $ map snd $ take n ws):bcs) (drop n ws)
          err idx msg = error $ msg ++ " (index " ++ show idx ++
                        ", value " ++ show (prog !! idx) ++ ")"

-- | parse hex to bytecode rep.
parseHex :: String -> [ByteCode]
parseHex = parse . hexToWord8s


type U256 = Word256
type S256 = Int256


w8sToU256 :: [Word8] -> U256
w8sToU256 = fst. foldr acc (0,0)
    where acc v (t,p) = (t + shift (fromIntegral v) p, p + 8)

data ByteCode =
          Inst Instruction
        | PushV U256
          deriving (Eq)

infixr 8 +>

(+>) :: (ToByteCode a,ToByteCode b) => a -> b -> [ByteCode]
a +> b = toByteCode a ++ toByteCode b

wPUSH :: U256 -> [ByteCode]
wPUSH = (Inst PUSH1:) . return . PushV

class ToByteCode a where toByteCode :: a -> [ByteCode]
instance ToByteCode Instruction where toByteCode = return . Inst
instance ToByteCode ByteCode where toByteCode = return
instance ToByteCode [ByteCode] where toByteCode = id

instance Show ByteCode where
    show (Inst i) = show i
    show (PushV w) = show w

data ParamSpec =
          Empty
        | Push Int
        | Dup Int
        | Swap Int
        | Log Int
        deriving (Eq,Show)

data Spec = Spec {
      value :: Word8,
      stackIn :: Int,
      stackOut :: Int,
      paramSpec :: ParamSpec
    } deriving Show

spec :: Instruction -> Spec
spec STOP = Spec 0x00 0 0 Empty
spec ADD = Spec 0x01 2 1 Empty
spec MUL = Spec 0x02 2 1 Empty
spec SUB = Spec 0x03 2 1 Empty
spec DIV = Spec 0x04 2 1 Empty
spec SDIV = Spec 0x05 2 1 Empty
spec MOD = Spec 0x06 2 1 Empty
spec SMOD = Spec 0x07 2 1 Empty
spec ADDMOD = Spec 0x08 3 1 Empty
spec MULMOD = Spec 0x09 3 1 Empty
spec EXP = Spec 0x0a 2 1 Empty
spec SIGNEXTEND = Spec 0x0b 2 1 Empty
spec LT = Spec 0x10 2 1 Empty
spec GT = Spec 0x11 2 1 Empty
spec SLT = Spec 0x12 2 1 Empty
spec SGT = Spec 0x13 2 1 Empty
spec EQ = Spec 0x14 2 1 Empty
spec ISZERO = Spec 0x15 1 1 Empty
spec AND = Spec 0x16 2 1 Empty
spec OR = Spec 0x17 2 1 Empty
spec XOR = Spec 0x18 2 1 Empty
spec NOT = Spec 0x19 1 1 Empty
spec BYTE = Spec 0x1a 2 1 Empty
spec SHA3 = Spec 0x20 2 1 Empty
spec ADDRESS = Spec 0x30 0 1 Empty
spec BALANCE = Spec 0x31 1 1 Empty
spec ORIGIN = Spec 0x32 0 1 Empty
spec CALLER = Spec 0x33 0 1 Empty
spec CALLVALUE = Spec 0x34 0 1 Empty
spec CALLDATALOAD = Spec 0x35 1 1 Empty
spec CALLDATASIZE = Spec 0x36 0 1 Empty
spec CALLDATACOPY = Spec 0x37 3 0 Empty
spec CODESIZE = Spec 0x38 0 1 Empty
spec CODECOPY = Spec 0x39 3 0 Empty
spec GASPRICE = Spec 0x3a 0 1 Empty
spec EXTCODESIZE = Spec 0x3b 1 1 Empty
spec EXTCODECOPY = Spec 0x3c 4 0 Empty
spec BLOCKHASH = Spec 0x40 1 1 Empty
spec COINBASE = Spec 0x41 0 1 Empty
spec TIMESTAMP = Spec 0x42 0 1 Empty
spec NUMBER = Spec 0x43 0 1 Empty
spec DIFFICULTY = Spec 0x44 0 1 Empty
spec GASLIMIT = Spec 0x45 0 1 Empty
spec POP = Spec 0x50 1 0 Empty
spec MLOAD = Spec 0x51 1 1 Empty
spec MSTORE = Spec 0x52 2 0 Empty
spec MSTORE8 = Spec 0x53 2 0 Empty
spec SLOAD = Spec 0x54 1 1 Empty
spec SSTORE = Spec 0x55 2 0 Empty
spec JUMP = Spec 0x56 1 0 Empty
spec JUMPI = Spec 0x57 2 0 Empty
spec PC = Spec 0x58 0 1 Empty
spec MSIZE = Spec 0x59 0 1 Empty
spec GAS = Spec 0x5a 0 1 Empty
spec JUMPDEST = Spec 0x5b 0 0 Empty
spec PUSH1 =  Spec  96 0 1 (Push 1)
spec PUSH2 =  Spec  97 0 1 (Push 2)
spec PUSH3 =  Spec  98 0 1 (Push 3)
spec PUSH4 =  Spec  99 0 1 (Push 4)
spec PUSH5 =  Spec 100 0 1 (Push 5)
spec PUSH6 =  Spec 101 0 1 (Push 6)
spec PUSH7 =  Spec 102 0 1 (Push 7)
spec PUSH8 =  Spec 103 0 1 (Push 8)
spec PUSH9 =  Spec 104 0 1 (Push 9)
spec PUSH10 = Spec 105 0 1 (Push 10)
spec PUSH11 = Spec 106 0 1 (Push 11)
spec PUSH12 = Spec 107 0 1 (Push 12)
spec PUSH13 = Spec 108 0 1 (Push 13)
spec PUSH14 = Spec 109 0 1 (Push 14)
spec PUSH15 = Spec 110 0 1 (Push 15)
spec PUSH16 = Spec 111 0 1 (Push 16)
spec PUSH17 = Spec 112 0 1 (Push 17)
spec PUSH18 = Spec 113 0 1 (Push 18)
spec PUSH19 = Spec 114 0 1 (Push 19)
spec PUSH20 = Spec 115 0 1 (Push 20)
spec PUSH21 = Spec 116 0 1 (Push 21)
spec PUSH22 = Spec 117 0 1 (Push 22)
spec PUSH23 = Spec 118 0 1 (Push 23)
spec PUSH24 = Spec 119 0 1 (Push 24)
spec PUSH25 = Spec 120 0 1 (Push 25)
spec PUSH26 = Spec 121 0 1 (Push 26)
spec PUSH27 = Spec 122 0 1 (Push 27)
spec PUSH28 = Spec 123 0 1 (Push 28)
spec PUSH29 = Spec 124 0 1 (Push 29)
spec PUSH30 = Spec 125 0 1 (Push 30)
spec PUSH31 = Spec 126 0 1 (Push 31)
spec PUSH32 = Spec 127 0 1 (Push 32)
spec DUP1 = Spec 128 0 1 (Dup 1)
spec DUP2 = Spec 129 0 1 (Dup 2)
spec DUP3 = Spec 130 0 1 (Dup 3)
spec DUP4 = Spec 131 0 1 (Dup 4)
spec DUP5 = Spec 132 0 1 (Dup 5)
spec DUP6 = Spec 133 0 1 (Dup 6)
spec DUP7 = Spec 134 0 1 (Dup 7)
spec DUP8 = Spec 135 0 1 (Dup 8)
spec DUP9 = Spec 136 0 1 (Dup 9)
spec DUP10 = Spec 137 0 1 (Dup 10)
spec DUP11 = Spec 138 0 1 (Dup 11)
spec DUP12 = Spec 139 0 1 (Dup 12)
spec DUP13 = Spec 140 0 1 (Dup 13)
spec DUP14 = Spec 141 0 1 (Dup 14)
spec DUP15 = Spec 142 0 1 (Dup 15)
spec DUP16 = Spec 143 0 1 (Dup 16)
spec SWAP1 = Spec 145 0 1 (Swap 1)
spec SWAP2 = Spec 146 0 1 (Swap 2)
spec SWAP3 = Spec 147 0 1 (Swap 3)
spec SWAP4 = Spec 148 0 1 (Swap 4)
spec SWAP5 = Spec 149 0 1 (Swap 5)
spec SWAP6 = Spec 150 0 1 (Swap 6)
spec SWAP7 = Spec 151 0 1 (Swap 7)
spec SWAP8 = Spec 152 0 1 (Swap 8)
spec SWAP9 = Spec 153 0 1 (Swap 9)
spec SWAP10 = Spec 154 0 1 (Swap 10)
spec SWAP11 = Spec 155 0 1 (Swap 11)
spec SWAP12 = Spec 156 0 1 (Swap 12)
spec SWAP13 = Spec 157 0 1 (Swap 13)
spec SWAP14 = Spec 158 0 1 (Swap 14)
spec SWAP15 = Spec 159 0 1 (Swap 15)
spec SWAP16 = Spec 160 0 1 (Swap 16)
spec LOG0 = Spec 0xa0 0 1 (Log 0)
spec LOG1 = Spec 0xa1 0 1 (Log 1)
spec LOG2 = Spec 0xa2 0 1 (Log 2)
spec LOG3 = Spec 0xa3 0 1 (Log 3)
spec LOG4 = Spec 0xa4 0 1 (Log 4)
spec CREATE = Spec 0xf0 3 1 Empty
spec CALL = Spec 0xf1 7 1 Empty
spec CALLCODE = Spec 0xf2 7 1 Empty
spec RETURN = Spec 0xf3 2 0 Empty
spec SUICIDE = Spec 0xff 1 0 Empty
