## Setup Cabal + GHC

https://www.haskell.org/ghcup

```
# Install ghcup
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# List installed components
ghcup list
```

### Run the Plutus Playground

```
# Checkout the Plutus version referencenced in cabal.project
cd ~/git/plutus \
  && git checkout ae35c4b8fe66dd626679bd2951bd72190e09a123 \
  && nix-shell

# Start the Plutus Playground Server
[nix-shell] cd plutus-playground-client && plutus-playground-server

# Start the Plutus Playground Client in another nix-shell
[nix-shell] cd plutus-playground-client && npm run start

https://localhost:8009
```

## Haskell Language Server

* HLS may continously crash with plutus pioneer program
* Use cabal-3.4.0.0 / ghc-8.10.4
* Complile hls with `executable-dynamic: True`

```
vi ~/.cabal/config
executable-dynamic: True

cabal clean # just to be sure
cabal build exe:haskell-language-server-wrapper
cabal install exe:haskell-language-server-wrapper --overwrite-policy=always
```

## Atom Haskell

https://atom-haskell.github.io

```
apm install language-haskell ide-haskell ide-haskell-cabal ide-haskell-hls
```

## Week 1

* Plutus contract comes in two parts: on chain, off chain (i.e. in the wallet)
* Date types can be shared between on/off chain parts
* Plutus script validates Tx and transfers funds locked by the script
* Wallet must be able to create Tx the fulfil all the conditions

Q: What is the ":: !" syntax in record data types?
A: It forces non-lazy evaluation of the record field

```
  data Auction = Auction
      { aSeller   :: !PubKeyHash
      , aDeadline :: !Slot
      , aMinBid   :: !Integer
      , aCurrency :: !CurrencySymbol
      , aToken    :: !TokenName
      } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)
```

## Week 2

https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/Data.hs

* Three pieces of data that a Plutus script gets
  1. Datum sitting at the UTXO
  2. Redeemer coming from the input under validation
  3. Context costing of the Tx inputs/outputs
* All three are of the same low level data type 'PlutusTx.Data' in plutus-tx

```
[week02]$ cabal repl

> import PlutusTx
> :i Data

type Data :: *
data Data
  = Constr Integer [Data]
  | Map [(Data, Data)]
  | List [Data]
  | I Integer
  | B ByteString
```

* We can trace error messages like this ...

```
mkValidator () (MyRedeemer b c) _ = traceIfFalse "wrong redeemer" $ b == c
```

## Week 3

https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Contexts.hs
https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Interval.hs
https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Slot.hs


* Lecture is concerned with 'Ledger.Contexts' in plutus-ledger-api
* ScriptContext is a record with two fields of type TxInfo, ScriptPurpose
* ScriptPurpose: Minting, Spending, Rewarding, Certifying

To obtain the wallet pubKeyHash we do ...

```
[week03]$ cabal repl

> import Wallet.Emulator
> import Ledger

> pubKeyHash $ walletPubKey $ Wallet 1
21fe31dfa154a261626bf854046fd2271b7bed4b6abe45aa58877ef47f9721b9

> pubKeyHash $ walletPubKey $ Wallet 2
39f713d0a644253f04529421b9f51b9b08979d08295959c4f3990ee617f5139f
```

Q: Clarify the roles for the off/on chain code (i.e. can on chain rely on off chain validation)
A: No, a Tx targeted for a script can be contructed in multiple ways

Q: Validator seems to be executed multiple times, once for each input locked by the script.
A: It gets called for every input locked by the script

Q: How is the Datum first initialised
A: Datum is associated with an Tx output targeting the script

```
==== Add slot 20 ====
Contract instance for wallet 1: (ReceiveEndpointCall (RawJson "{\"value\":{\"unEndpointValue\":[]},\"tag\":\"grab\"}"))
Contract instance for wallet 1: (ContractLog (RawJson "found 2 gift(s) to grab"))
Validation failed: f366dc51a2f48b0a34b0800e0ff7a319081593ddb324edabf3d914e3fdd22df5
 (ScriptFailure (EvaluationError ["Beneficiary1 has signed: False","Beneficiary2 has signed: True","AfterDeadline: False"]))
Validation failed: f366dc51a2f48b0a34b0800e0ff7a319081593ddb324edabf3d914e3fdd22df5
 (ScriptFailure (EvaluationError ["Beneficiary1 has signed: False","Beneficiary2 has signed: True","AfterDeadline: False"]))
```

## Week 4

https://github.com/tdiesler/plutus/blob/pioneer-program/plutus-contract/src/Plutus/Trace/Emulator.hs
https://github.com/tdiesler/plutus/blob/pioneer-program/plutus-contract/src/Wallet/Emulator/Stream.hs (EmulatorConfig)
https://github.com/tdiesler/plutus/blob/pioneer-program/plutus-contract/src/Plutus/Contract/Trace.hs (InitialDistribution)

* Off chain Contract, Wallet Monad
* Contract Monad defines codes that runs in a Wallet
* defaultDist(For) gives the default InitialDistribution

Q: [TODO] What is the difference between Contract.logInfo and Extras.logInfo
A: ???

```
[week04]$ cabal repl

> import Plutus.Trace.Emulator
> import Plutus.Contract.Trace
> import Wallet.Emulator.Stream

> defaultDistFor [Wallet 1]
fromList [(Wallet 1,Value (Map [(,Map [("",100000000)])]))]

# Create an EmulatorConfig
> ecfg = EmulatorConfig $ Left defaultDist
> runEmulatorTrace ecfg $ return ()

> runEmulatorTraceIO $ return ()
```

* For a simple trace see Trace.hs
* For simple contracts see Contract.hs
* OverloadedStrings allows you to use "string" as Text
* TypeApplications allows to use @String "foo" instead of ("foo" :: String)
* BlockchainActions doesn't have support for specific endpoints

## Week 5

https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Ada.hs
https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Value.hs

* Each Value is identified by a CurrecySymbol and a TokenName
* An AssetClass is a wrapper around a CurrecySymbol and a TokenName


```
# Working with ADA
> :set -XOverloadedStrings
> import Plutus.V1.Ledger.Ada
> import Plutus.V1.Ledger.Value
> lovelaceValueOf 123
Value (Map [(,Map [("",123)])])
> lovelaceValueOf 123 `mappend` lovelaceValueOf 10
> lovelaceValueOf 123 <> lovelaceValueOf 10

# Working with Tokens
> flattenValue $ singleton "af08" "abc" 10 <> singleton "af08" "abc" 7 <> lovelaceValueOf 123
[(af08,"abc",17),(,"",123)]

```

## Week 6

* Plutus of the given version may have to be build first (i.e. run nix-shell in plutus)
* PubKeyHash of the contract owner is part of the Datum
* There is a findDatum on TxInfo
* Off-Chain startOracle mints the NFT
* Checkout Plutus.Contract.mapError to error type in the Contract monad
* pack :: String -> Text

## Week 8

* [Plutus API documentation](https://docs.plutus-community.com/docs/setup/buildDocumentation.html)

Q: [TODO] Can we provision a script address with funds in the EmulatorConfig?
A:

Q: [TODO] Can we assert intermediary wallet state in a test workflow?
A:

Q: [TODO] How can we assert the final script UTxOs?
A:
