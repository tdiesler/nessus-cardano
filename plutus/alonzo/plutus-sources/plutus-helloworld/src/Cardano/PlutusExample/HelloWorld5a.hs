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


globalMapping :: [(PubKeyHash, P.ByteString)]
globalMapping = [
  ("cd117d5746886fc8359c09856414783b726d2868a6f43d65ef9f57ff", "secret1"),
  ("b1dfb0f0df030c0af70f26eaf5928637bdb3fe169f6fd045e59ce95a", "secret2")]

{-
   The Hello World validator script
-}

{-# INLINABLE helloWorld #-}

helloWorld :: [(PubKeyHash, P.ByteString)] -> Integer -> P.ByteString -> ScriptContext -> P.Bool
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

getPubKeyHashFromMapping :: [(PubKeyHash, P.ByteString)] -> Integer -> Maybe PubKeyHash
getPubKeyHashFromMapping mapping idx = Just $ P.fst $ mapping P.!! idx

getSecretFromMapping :: [(PubKeyHash, P.ByteString)] -> Integer -> P.ByteString
getSecretFromMapping mapping idx = P.snd $ mapping P.!! idx

{-
    As a ScriptInstance
-}

data HelloWorld
instance Scripts.ValidatorTypes HelloWorld where
    type instance DatumType HelloWorld = Integer
    type instance RedeemerType HelloWorld = P.ByteString

helloWorldInstance :: Scripts.TypedValidator HelloWorld
helloWorldInstance = Scripts.mkTypedValidator @HelloWorld
    ($$(PlutusTx.compile [|| helloWorld ||]) `PlutusTx.applyCode` PlutusTx.liftCode globalMapping)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @P.ByteString

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
