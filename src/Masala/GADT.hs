{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Masala.GADT where

import GHC.TypeLits

-- | Cmd with bytecode value, stack consumed, stack produced
data Cmd ::  Nat ->  Nat -> Nat -> * where
        STOP :: Cmd 0x00  0  0 
        ADD :: Cmd 0x01  2  1 
        MUL :: Cmd 0x02  2  1 
        SUB :: Cmd 0x03  2  1 
        DIV :: Cmd 0x04  2  1 
        SDIV :: Cmd 0x05  2  1 
        MOD :: Cmd 0x06  2  1 
        SMOD :: Cmd 0x07  2  1 
        ADDMOD :: Cmd 0x08  3  1 
        MULMOD :: Cmd 0x09  3  1 
        EXP :: Cmd 0x0a  2  1 
        SIGNEXTEND :: Cmd 0x0b  2  1 
        LT :: Cmd 0x10  2  1 
        GT :: Cmd 0x11  2  1 
        SLT :: Cmd 0x12  2  1 
        SGT :: Cmd 0x13  2  1 
        EQ :: Cmd 0x14  2  1 
        ISZERO :: Cmd 0x15  1  1 
        AND :: Cmd 0x16  2  1 
        OR :: Cmd 0x17  2  1 
        XOR :: Cmd 0x18  2  1 
        NOT :: Cmd 0x19  1  1 
        BYTE :: Cmd 0x1a  2  1 
        SHA3 :: Cmd 0x20  2  1 
        ADDRESS :: Cmd 0x30  0  1 
        BALANCE :: Cmd 0x31  1  1 
        ORIGIN :: Cmd 0x32  0  1 
        CALLER :: Cmd 0x33  0  1 
        CALLVALUE :: Cmd 0x34  0  1 
        CALLDATALOAD :: Cmd 0x35  1  1 
        CALLDATASIZE :: Cmd 0x36  0  1 
        CALLDATACOPY :: Cmd 0x37  3  0 
        CODESIZE :: Cmd 0x38  0  1 
        CODECOPY :: Cmd 0x39  3  0 
        GASPRICE :: Cmd 0x3a  0  1 
        EXTCODESIZE :: Cmd 0x3b  1  1 
        EXTCODECOPY :: Cmd 0x3c  4  0 
        BLOCKHASH :: Cmd 0x40  1  1 
        COINBASE :: Cmd 0x41  0  1 
        TIMESTAMP :: Cmd 0x42  0  1 
        NUMBER :: Cmd 0x43  0  1 
        DIFFICULTY :: Cmd 0x44  0  1 
        GASLIMIT :: Cmd 0x45  0  1 
        POP :: Cmd 0x50  1  0 
        MLOAD :: Cmd 0x51  1  1 
        MSTORE :: Cmd 0x52  2  0 
        MSTORE8 :: Cmd 0x53  2  0 
        SLOAD :: Cmd 0x54  1  1 
        SSTORE :: Cmd 0x55  2  0 
        JUMP :: Cmd 0x56  1  0 
        JUMPI :: Cmd 0x57  2  0 
        PC :: Cmd 0x58  0  1 
        MSIZE :: Cmd 0x59  0  1 
        GAS :: Cmd 0x5a  0  1 
        JUMPDEST :: Cmd 0x5b  0  0 
        PUSH :: Int -> Cmd 0x60  0  1 
        DUP :: Int -> Cmd 0x80  1  1 
        SWAP :: Int -> Cmd 0x90  2  0 
        LOG :: Int -> Cmd 0xa0  2  0 
        CREATE :: Cmd 0xf0  3  1 
        CALL :: Cmd 0xf1  7  1 
        CALLCODE :: Cmd 0xf2  7  1 
        RETURN :: Cmd 0xf3  2  0 
        SUICIDE :: Cmd 0xff  1  0 
        
