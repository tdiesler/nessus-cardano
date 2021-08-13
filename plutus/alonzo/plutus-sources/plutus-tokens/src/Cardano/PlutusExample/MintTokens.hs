{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- This solution has been contributed by George Flerovsky

module Cardano.PlutusExample.MintTokens
  ( mintTokensSBS
  , mintTokensSerialised
  ) where

import           Cardano.Api.Shelley    (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Short  as SBS
import qualified Data.ByteString.Lazy   as LBS
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value

{-# INLINABLE mintTokens #-}
mintTokens :: Integer -> ScriptContext -> Bool
mintTokens amt ctx = traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    cs :: CurrencySymbol
    cs = ownCurrencySymbol ctx

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoForge info) of
        [(cs', _, amt')] -> cs' == cs && amt' == amt
        _                  -> False

mintingPolicy :: Scripts.MintingPolicy
mintingPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mintTokens ||])

{-
    As a Script
-}

mintTokensScript :: Script
mintTokensScript = unMintingPolicyScript mintingPolicy

{-
    As a Short Byte String
-}

mintTokensSBS :: SBS.ShortByteString
mintTokensSBS =  SBS.toShort . LBS.toStrict $ serialise mintTokensScript

{-
    As a Serialised Script
-}

mintTokensSerialised :: PlutusScript PlutusScriptV1
mintTokensSerialised = PlutusScriptSerialised mintTokensSBS
