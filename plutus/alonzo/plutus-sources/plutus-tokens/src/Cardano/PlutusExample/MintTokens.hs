{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

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

data MintingParams = MintingParams {
  prmsPubKeyHash  :: PubKeyHash
}

mintingParams :: MintingParams
mintingParams = MintingParams {
  -- PubKeyHash ~/cardano/keys/testnet/acc1/payment.pkh
  prmsPubKeyHash = "36deb53fa63e507df19b5cd69bc1f0d2a214e3d738b68883fb27e10f"
}

PlutusTx.makeLift ''MintingParams
PlutusTx.unstableMakeIsData ''MintingParams

{-# INLINABLE mintTokens #-}
mintTokens :: MintingParams -> Integer -> ScriptContext -> Bool
mintTokens prms amt ctx =
    traceIfFalse "wrong currency symbol" checkMintedSymbol
    && traceIfFalse "wrong amount minted" checkMintedAmount
    -- && traceIfFalse "owner has not signed" checkOwnerHasSigned
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkMintedSymbol :: Bool
    checkMintedSymbol = case flattenValue (txInfoMint info) of
        [(cs', _, _)] -> cs' == ownCurrencySymbol ctx
        _             -> False

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, _, amt')] -> amt' == amt
        _              -> False

    hasSigned :: PubKeyHash -> Bool
    hasSigned pkh = pkh `elem` txInfoSignatories info

    checkOwnerHasSigned :: Bool
    checkOwnerHasSigned = hasSigned (prmsPubKeyHash prms)

{-
    As Minting Policy
-}

mintingPolicy :: Scripts.MintingPolicy
mintingPolicy = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mintTokens ||])
    `PlutusTx.applyCode` PlutusTx.liftCode mintingParams

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
