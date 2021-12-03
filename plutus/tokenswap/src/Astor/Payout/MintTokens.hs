{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Astor.Payout.MintTokens
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
  -- cat ~/cardano/testnet/keys/acc0/payment.pkh
  --prmsPubKeyHash = "b16634c241055f4a8cd95e0fe903b015eed641d0590aedaf780eb6c9"
  -- cat ~/cardano/mainnet/keys/acc0/payment.pkh
  prmsPubKeyHash = "4e38086416d9c5bc6ef667b36265ff7659f93b0e217ba456d1dbf5fe"
}

PlutusTx.makeLift ''MintingParams
PlutusTx.unstableMakeIsData ''MintingParams

{-# INLINABLE mintTokens #-}
mintTokens :: MintingParams -> Integer -> ScriptContext -> Bool
mintTokens mprms amt ctx =
    traceIfFalse "Unexpected currency symbol" checkMintedSymbol
    && traceIfFalse "Invalid minting amount" checkMintedAmount
    && traceIfFalse "Owner has not signed" ownerHasSigned
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownerPubKeyHash :: PubKeyHash
    ownerPubKeyHash = prmsPubKeyHash mprms

    checkMintedSymbol :: Bool
    checkMintedSymbol = case flattenValue (txInfoMint info) of
        [(cs', _, _)] -> cs' == ownCurrencySymbol ctx
        _             -> False

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, _, amt')] -> amt' == amt
        _              -> False

    ownerHasSigned :: Bool
    ownerHasSigned = hasPubKeyHashSigned ownerPubKeyHash ctx

-- =============================================================================
-- On-Chain utility functions

-- Get the list of input addresses
--
getInputAddresses :: ScriptContext -> [Address]
getInputAddresses ctx = let
    info = scriptContextTxInfo ctx
    inaddrs = [txOutAddress txo | TxInInfo{txInInfoResolved=txo} <- txInfoInputs info]
  in inaddrs

-- True if the given PubKeyHash has signed
--
hasPubKeyHashSigned :: PubKeyHash -> ScriptContext -> Bool
hasPubKeyHashSigned pkh ctx = checkHasPubKeyHashSigned
  where
    inputPubKeyHashes :: [Maybe PubKeyHash]
    inputPubKeyHashes = map toPubKeyHash $ getInputAddresses ctx
    checkHasPubKeyHashSigned = Just pkh `elem` inputPubKeyHashes

-- =============================================================================

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
