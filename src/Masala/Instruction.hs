{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Masala instruction/opcode declaration, specification and parsing.
module Masala.Instruction
    ( -- * Data types
     ByteCode (..)
    ,ToByteCode (..), (+>), wPUSH
    ,ParamSpec (..)
    ,Spec (..)
    ,Instruction (..)
    ,parse, parseHex
    ,bcToHex
    ,bcToWord8s,bcsToWord8s
    ,spec
) where

import Data.Word
import Prelude hiding (LT,GT,EQ)
import qualified Data.Map as M
import Masala.Word


-- | ByteCode representation for VM, optimized for PUSHx operations.
data ByteCode = ByteCode {
      -- | Index/address, from source byte sequence.
      bcIdx :: Int
      -- | Instruction/opcode
    , bcInst :: Instruction
      -- | Push values, if any
    , bcValue :: [Word8]
    } deriving (Eq)
instance Show ByteCode where
    show (ByteCode n i w) = show n ++ ":" ++ show i ++ if null w then "" else show w

-- | Convenience class for repl, mainly.
class ToByteCode a where
    toByteCode :: a -> [ByteCode]
    -- | In GHCI, issue "default (U256)" to enter bare numbers for push.
    default toByteCode :: (Bits a ,Integral a) => a -> [ByteCode]
    toByteCode a = return $ wPUSH $ fromIntegral a

instance ToByteCode U256
instance ToByteCode Instruction where toByteCode i = return $ ByteCode 0 i []
instance ToByteCode ByteCode where toByteCode = return
instance ToByteCode [ByteCode] where toByteCode = id

-- | Parameterize PUSHxx, SWAPxx etc
data ParamSpec =
    -- | PUSH1 => PushW 1, etc
    PushW Int |
    -- | DUP2 => Dup 2 ...
    Dup Int |
    -- | SWAPxxx
    Swap Int |
    -- | LOGxxx
    Log Int
        deriving (Eq,Show)

-- | Instruction/opcode specification, per yellow paper
data Spec = Spec {
      -- | Opcode
      value :: Word8,
      -- | How many words to pop off stack
      stackIn :: Int,
      -- | How many words will be pushed onto stack
      stackOut :: Int,
      -- | parameterization, if any
      paramSpec :: Maybe ParamSpec
    } deriving Show


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

-- | parse Word8s to bytecode rep.
parse :: [Word8] -> Either String [ByteCode]
parse prog = toBC [] . zip [0..] $ prog
    where toBC bcs [] = Right $ reverse bcs
          toBC bcs ((idx,v):ws) =
              case M.lookup v valueToInst of
                Nothing -> err idx $ "Instruction expected, parsed: " ++ show (reverse bcs)
                Just i ->
                    case paramSpec (spec i) of
                      (Just (PushW n)) -> push i idx n bcs ws
                      _ -> toBC (ByteCode idx i []:bcs) ws
          push inst idx n bcs ws
              | n > length ws =
                  err idx ("PUSH" ++show n ++ ": not enough input")
              | otherwise =
                  toBC (ByteCode idx inst (map snd $ take n ws):bcs) (drop n ws)
          err idx msg = Left $ msg ++ " (index " ++ show idx ++
                        ", value " ++ show (prog !! idx) ++ ")"

-- | parse hex to bytecode rep.
parseHex :: String -> Either String [ByteCode]
parseHex = either Left parse . readHexs

-- | back to hex string
bcToHex :: [ByteCode] -> String
bcToHex = showHexs . concatMap bcToWord8s

infixl 8 +>

-- | convenience for GHCI. With "(default U256)", can write "0x1010 +> 0x11 +> ADD" in ghci, etc.
(+>) :: (ToByteCode a,ToByteCode b) => a -> b -> [ByteCode]
a +> b = ba ++ setIdxs(toByteCode b)
    where ba = toByteCode a
          prevIdx = if null ba then 0 else idx . head $ reverse ba
          idx (ByteCode n _ w) = n + length w
          setIdxs = zipWith setIdx [succ prevIdx..]
          setIdx i bc = bc { bcIdx = i }

-- | convenience for entering push values. Can also use "(default U256)".
wPUSH :: U256 -> ByteCode
wPUSH v = ByteCode 0 selectPush ws
    where ws = u256ToW8s v
          selectPush = [pred PUSH1 ..] !! length ws


-- | Bytecode is optimized for PUSH, meaning it's not one-to-one with opcode sequences.
bcToWord8s :: ByteCode -> [Word8]
bcToWord8s (ByteCode _ i []) = return $ value $ spec i
bcToWord8s (ByteCode _ i ws) = value (spec i):ws

-- | Convert back to words.
bcsToWord8s :: [ByteCode] -> [Word8]
bcsToWord8s = concatMap bcToWord8s

-- | Opcode specification, generated from yellow paper.
spec :: Instruction -> Spec
spec STOP = Spec 0x00 0 0 Nothing
spec ADD = Spec 0x01 2 1 Nothing
spec MUL = Spec 0x02 2 1 Nothing
spec SUB = Spec 0x03 2 1 Nothing
spec DIV = Spec 0x04 2 1 Nothing
spec SDIV = Spec 0x05 2 1 Nothing
spec MOD = Spec 0x06 2 1 Nothing
spec SMOD = Spec 0x07 2 1 Nothing
spec ADDMOD = Spec 0x08 3 1 Nothing
spec MULMOD = Spec 0x09 3 1 Nothing
spec EXP = Spec 0x0a 2 1 Nothing
spec SIGNEXTEND = Spec 0x0b 2 1 Nothing
spec LT = Spec 0x10 2 1 Nothing
spec GT = Spec 0x11 2 1 Nothing
spec SLT = Spec 0x12 2 1 Nothing
spec SGT = Spec 0x13 2 1 Nothing
spec EQ = Spec 0x14 2 1 Nothing
spec ISZERO = Spec 0x15 1 1 Nothing
spec AND = Spec 0x16 2 1 Nothing
spec OR = Spec 0x17 2 1 Nothing
spec XOR = Spec 0x18 2 1 Nothing
spec NOT = Spec 0x19 1 1 Nothing
spec BYTE = Spec 0x1a 2 1 Nothing
spec SHA3 = Spec 0x20 2 1 Nothing
spec ADDRESS = Spec 0x30 0 1 Nothing
spec BALANCE = Spec 0x31 1 1 Nothing
spec ORIGIN = Spec 0x32 0 1 Nothing
spec CALLER = Spec 0x33 0 1 Nothing
spec CALLVALUE = Spec 0x34 0 1 Nothing
spec CALLDATALOAD = Spec 0x35 1 1 Nothing
spec CALLDATASIZE = Spec 0x36 0 1 Nothing
spec CALLDATACOPY = Spec 0x37 3 0 Nothing
spec CODESIZE = Spec 0x38 0 1 Nothing
spec CODECOPY = Spec 0x39 3 0 Nothing
spec GASPRICE = Spec 0x3a 0 1 Nothing
spec EXTCODESIZE = Spec 0x3b 1 1 Nothing
spec EXTCODECOPY = Spec 0x3c 4 0 Nothing
spec BLOCKHASH = Spec 0x40 1 1 Nothing
spec COINBASE = Spec 0x41 0 1 Nothing
spec TIMESTAMP = Spec 0x42 0 1 Nothing
spec NUMBER = Spec 0x43 0 1 Nothing
spec DIFFICULTY = Spec 0x44 0 1 Nothing
spec GASLIMIT = Spec 0x45 0 1 Nothing
spec POP = Spec 0x50 1 0 Nothing
spec MLOAD = Spec 0x51 1 1 Nothing
spec MSTORE = Spec 0x52 2 0 Nothing
spec MSTORE8 = Spec 0x53 2 0 Nothing
spec SLOAD = Spec 0x54 1 1 Nothing
spec SSTORE = Spec 0x55 2 0 Nothing
spec JUMP = Spec 0x56 1 0 Nothing
spec JUMPI = Spec 0x57 2 0 Nothing
spec PC = Spec 0x58 0 1 Nothing
spec MSIZE = Spec 0x59 0 1 Nothing
spec GAS = Spec 0x5a 0 1 Nothing
spec JUMPDEST = Spec 0x5b 0 0 Nothing
spec PUSH1 =  Spec  96 0 1 (Just $ PushW 1)
spec PUSH2 =  Spec  97 0 1 (Just $ PushW 2)
spec PUSH3 =  Spec  98 0 1 (Just $ PushW 3)
spec PUSH4 =  Spec  99 0 1 (Just $ PushW 4)
spec PUSH5 =  Spec 100 0 1 (Just $ PushW 5)
spec PUSH6 =  Spec 101 0 1 (Just $ PushW 6)
spec PUSH7 =  Spec 102 0 1 (Just $ PushW 7)
spec PUSH8 =  Spec 103 0 1 (Just $ PushW 8)
spec PUSH9 =  Spec 104 0 1 (Just $ PushW 9)
spec PUSH10 = Spec 105 0 1 (Just $ PushW 10)
spec PUSH11 = Spec 106 0 1 (Just $ PushW 11)
spec PUSH12 = Spec 107 0 1 (Just $ PushW 12)
spec PUSH13 = Spec 108 0 1 (Just $ PushW 13)
spec PUSH14 = Spec 109 0 1 (Just $ PushW 14)
spec PUSH15 = Spec 110 0 1 (Just $ PushW 15)
spec PUSH16 = Spec 111 0 1 (Just $ PushW 16)
spec PUSH17 = Spec 112 0 1 (Just $ PushW 17)
spec PUSH18 = Spec 113 0 1 (Just $ PushW 18)
spec PUSH19 = Spec 114 0 1 (Just $ PushW 19)
spec PUSH20 = Spec 115 0 1 (Just $ PushW 20)
spec PUSH21 = Spec 116 0 1 (Just $ PushW 21)
spec PUSH22 = Spec 117 0 1 (Just $ PushW 22)
spec PUSH23 = Spec 118 0 1 (Just $ PushW 23)
spec PUSH24 = Spec 119 0 1 (Just $ PushW 24)
spec PUSH25 = Spec 120 0 1 (Just $ PushW 25)
spec PUSH26 = Spec 121 0 1 (Just $ PushW 26)
spec PUSH27 = Spec 122 0 1 (Just $ PushW 27)
spec PUSH28 = Spec 123 0 1 (Just $ PushW 28)
spec PUSH29 = Spec 124 0 1 (Just $ PushW 29)
spec PUSH30 = Spec 125 0 1 (Just $ PushW 30)
spec PUSH31 = Spec 126 0 1 (Just $ PushW 31)
spec PUSH32 = Spec 127 0 1 (Just $ PushW 32)
spec DUP1 = Spec 128 0 1 (Just $ Dup 1)
spec DUP2 = Spec 129 0 1 (Just $ Dup 2)
spec DUP3 = Spec 130 0 1 (Just $ Dup 3)
spec DUP4 = Spec 131 0 1 (Just $ Dup 4)
spec DUP5 = Spec 132 0 1 (Just $ Dup 5)
spec DUP6 = Spec 133 0 1 (Just $ Dup 6)
spec DUP7 = Spec 134 0 1 (Just $ Dup 7)
spec DUP8 = Spec 135 0 1 (Just $ Dup 8)
spec DUP9 = Spec 136 0 1 (Just $ Dup 9)
spec DUP10 = Spec 137 0 1 (Just $ Dup 10)
spec DUP11 = Spec 138 0 1 (Just $ Dup 11)
spec DUP12 = Spec 139 0 1 (Just $ Dup 12)
spec DUP13 = Spec 140 0 1 (Just $ Dup 13)
spec DUP14 = Spec 141 0 1 (Just $ Dup 14)
spec DUP15 = Spec 142 0 1 (Just $ Dup 15)
spec DUP16 = Spec 143 0 1 (Just $ Dup 16)
spec SWAP1 = Spec 144 0 1 (Just $ Swap 1)
spec SWAP2 = Spec 145 0 1 (Just $ Swap 2)
spec SWAP3 = Spec 146 0 1 (Just $ Swap 3)
spec SWAP4 = Spec 147 0 1 (Just $ Swap 4)
spec SWAP5 = Spec 148 0 1 (Just $ Swap 5)
spec SWAP6 = Spec 149 0 1 (Just $ Swap 6)
spec SWAP7 = Spec 150 0 1 (Just $ Swap 7)
spec SWAP8 = Spec 151 0 1 (Just $ Swap 8)
spec SWAP9 = Spec 152 0 1 (Just $ Swap 9)
spec SWAP10 = Spec 153 0 1 (Just $ Swap 10)
spec SWAP11 = Spec 154 0 1 (Just $ Swap 11)
spec SWAP12 = Spec 155 0 1 (Just $ Swap 12)
spec SWAP13 = Spec 156 0 1 (Just $ Swap 13)
spec SWAP14 = Spec 157 0 1 (Just $ Swap 14)
spec SWAP15 = Spec 158 0 1 (Just $ Swap 15)
spec SWAP16 = Spec 159 0 1 (Just $ Swap 16)
spec LOG0 = Spec 0xa0 2 1 (Just $ Log 0)
spec LOG1 = Spec 0xa1 3 1 (Just $ Log 1)
spec LOG2 = Spec 0xa2 4 1 (Just $ Log 2)
spec LOG3 = Spec 0xa3 5 1 (Just $ Log 3)
spec LOG4 = Spec 0xa4 6 1 (Just $ Log 4)
spec CREATE = Spec 0xf0 3 1 Nothing
spec CALL = Spec 0xf1 7 1 Nothing
spec CALLCODE = Spec 0xf2 7 1 Nothing
spec RETURN = Spec 0xf3 2 0 Nothing
spec SUICIDE = Spec 0xff 1 0 Nothing
