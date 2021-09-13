{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- This solution has been contributed by George Flerovsky

module Cardano.PlutusExample.HelloWorld5a
  ( globalMapping
  , helloWorldSerialised
  , helloWorldSBS
  ) where

import           Prelude hiding (($))

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS

import           Ledger               hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude     as P hiding (Semigroup (..), unless)


globalMapping :: [(PubKeyHash, P.BuiltinByteString)]
globalMapping = [
  ("36deb53fa63e507df19b5cd69bc1f0d2a214e3d738b68883fb27e10f", "secret1"),
  ("071c6180a8fd2b486b9f40a1363ac0717518ab305ec3db54f5268ae8", "secret2"),
  ("f9d84b9ee7f753478b5b86be5fadfa8a2b1144ba087bc1b8a1023d18", "secret3")]

{-
   The Hello World validator script
-}

{-# INLINABLE helloWorld #-}

helloWorld :: [(PubKeyHash, P.BuiltinByteString)] -> Integer -> P.BuiltinByteString -> ScriptContext -> P.Bool
helloWorld mapping _ redeemer ctx = let
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txouts :: [TxOut]
    txouts = txInfoOutputs info

    outPubKeyHash :: Maybe PubKeyHash
    outPubKeyHash = toPubKeyHash $ txOutAddress $ P.head txouts

    isOutWallet1 = outPubKeyHash P.== getPubKeyHashFromMapping mapping 0
    isOutWallet2 = outPubKeyHash P.== getPubKeyHashFromMapping mapping 1

    isSecret1 = redeemer P.== getSecretFromMapping mapping 0
    isSecret2 = redeemer P.== getSecretFromMapping mapping 1

  in isOutWallet1 P.&& isSecret1 P.|| isOutWallet2 P.&& isSecret2

getPubKeyHashFromMapping :: [(PubKeyHash, P.BuiltinByteString)] -> Integer -> Maybe PubKeyHash
getPubKeyHashFromMapping mapping idx = Just $ P.fst $ mapping P.!! idx

getSecretFromMapping :: [(PubKeyHash, P.BuiltinByteString)] -> Integer -> P.BuiltinByteString
getSecretFromMapping mapping idx = P.snd $ mapping P.!! idx

{-
    As a ScriptInstance
-}

data HelloWorld
instance Scripts.ValidatorTypes HelloWorld where
    type instance DatumType HelloWorld = Integer
    type instance RedeemerType HelloWorld = P.BuiltinByteString

helloWorldInstance :: Scripts.TypedValidator HelloWorld
helloWorldInstance = Scripts.mkTypedValidator @HelloWorld
    ($$(PlutusTx.compile [|| helloWorld ||]) `PlutusTx.applyCode` PlutusTx.liftCode globalMapping)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @P.BuiltinByteString

{-
    As a Validator
-}

helloWorldValidator :: Validator
helloWorldValidator = Scripts.validatorScript helloWorldInstance


{-
    As a Script
-}

helloWorldScript :: Script
helloWorldScript = unValidatorScript helloWorldValidator

{-
    As a Short Byte String
-}

helloWorldSBS :: SBS.ShortByteString
helloWorldSBS =  SBS.toShort . LBS.toStrict $ serialise helloWorldScript

{-
    As a Serialised Script
-}

helloWorldSerialised :: PlutusScript PlutusScriptV1
helloWorldSerialised = PlutusScriptSerialised helloWorldSBS
