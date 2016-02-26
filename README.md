Masala: Pure Ethereum VM and RPC with flexible backends, gas model
==================================================================

Implementation of Ethereum VM, to support investigations into hosting EVM and bytecode
in a pure consensus-backed ledger such as Juno.

VM/Bytecode interpreter
-----------------------

`Masala.VM` houses the main interpreter, which sports a tight dispatch driven by the
yellow paper spec ensuring that the stack interop is correct.

Showcases Haskell's superior numeric flexibility by leveraging
`Word256`, `Int256`, `Word8` and `Word160` to represent the main EVM
datatypes, with the respective newtypes `U256`, `S256`, `U8` and
`Address` supporting automatic JSON conversion.

Gas Models
----------

Supports ethereum's complex gas model, as well as a `FixedGasModel` of one-unit-per-instruction.

Backend Polymorphism
--------------------

VM Backend is represented as the `MonadExt` typeclass, with a basic implementation. This can
easily be extended to write to a database, etc.

Because it's a transformer typeclass, it easily hoists into the RPC monad or runs standalone.
Note however the transformer implementations only allow it to be the "inner" monad on the stack.

REPL/RPC
--------

To run repl in GHCI, load module 'Masala.Repl', issue `_repl`, and enter RPC JSON calls.

```
Solidity code:
contract SimpleStorage2 {
   uint storedData;
   function set(uint x) {
       storedData = x;
   }
   function get() constant returns (uint retVal) {
       return storedData;
   }
}
```

```
ghci> :load Masala.Repl
Ok, modules loaded: Masala.VM, Masala.Instruction, Masala.Word, Masala.Ext.Simple, Masala.VM.Types, Masala.VM.Dispatch, Masala.VM.Memory, Masala.VM.Gas, Masala.RPC, Masala.Repl.
ghci> _repl
> {"method":"eth_sendTransaction","params":[{"from":"1e240","data":"0x606060405260908060106000396000f360606040526000357c01000000000000000000000000000000000000000000000000000000009004806360fe47b11460415780636d4ce63c14605257603f565b005b60506004803590602001506071565b005b605b600450607f565b6040518082815260200191505060405180910390f35b806000600050819055505b50565b60006000600050549050608d565b9056"}]}
sendTransaction: SendTran {stfrom = 1e240, stto = Nothing, stgas = Nothing, stgasPrice = Nothing, stvalue = Nothing, stdata = Just 0x606060405260908060106000396000f360606040526000357c01000000000000000000000000000000000000000000000000000000009004806360fe47b11460415780636d4ce63c14605257603f565b005b60506004803590602001506071565b005b605b600450607f565b6040518082815260200191505060405180910390f35b806000600050819055505b50565b60006000600050549050608d565b9056, stnonce = Nothing}
[...]
sendTransaction: Success, addr=1e241, output=[60,60,60,40,52,60,0,35,7c,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,90,4,80,63,60,fe,47,b1,14,60,41,57,80,63,6d,4c,e6,3c,14,60,52,57,60,3f,56,5b,0,5b,60,50,60,4,80,35,90,60,20,1,50,60,71,56,5b,0,5b,60,5b,60,4,50,60,7f,56,5b,60,40,51,80,82,81,52,60,20,1,91,50,50,60,40,51,80,91,3,90,f3,5b,80,60,0,60,0,50,81,90,55,50,5b,50,56,5b,60,0,60,0,60,0,50,54,90,50,60,8d,56,5b,90,56]
"Success, addr=1e241, acct=Just ExtAccount {code=*bytecode*, bal=0, addy=1e241, store=fromList []"
> {"method":"eth_call","params":[{"to":"1e241","data":"0x60fe47b1000000000000000000000000000000000000000000000000000000000000cafe"},"0x0"]}
[...]
call: Success, output=
> {"method":"eth_call","params":[{"to":"1e241","data":"0x6d4ce63c00000000000000000000000000000000000000000000000000000000"},"0x0"]}
[...]
call: Success, output=000000000000000000000000000000000000000000000000000000000000cafe
```

The repl implements an IORef state model suitable for plugging into a consensus state machine like Juno.

JSON Test Suite
---------------

A subset of the go and cpp tests (circa December 2015) are included, which are executed as an HSpec test.

Note that I extended the JSON format to include a "skip" key, for the
tests which are intentionally unsupported. This includes some quirks
of the go code test fixture, i.e. creating accounts on overflow, etc.
