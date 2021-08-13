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

module Cardano.PlutusExample.NFT where

import           Cardano.Api.Shelley    (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Short  as SBS
import qualified Data.ByteString.Lazy   as LBS
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value

tname :: TokenName
tname = "MyNFT"

{-# INLINABLE mintTokens #-}
mintTokens :: TokenName -> () -> ScriptContext -> Bool
mintTokens tn () ctx = traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoForge info) of
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
        _                -> False

mintingPolicy :: Scripts.MintingPolicy
mintingPolicy = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mintTokens ||])
      `PlutusTx.applyCode` PlutusTx.liftCode tname

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
