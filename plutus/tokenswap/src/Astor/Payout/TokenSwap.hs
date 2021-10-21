{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Astor.Payout.TokenSwap
  ( tokenSwapSerialised
  , tokenSwapSBS
  ) where

--import           Prelude (length)
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Short  as SBS
import qualified Data.ByteString.Lazy   as LBS

import           Ledger                 hiding (singleton)
import qualified Ledger.Typed.Scripts   as Scripts

import qualified PlutusTx
import           PlutusTx.Prelude       as P hiding (Semigroup (..), unless)
import           Plutus.V1.Ledger.Ada
import           Plutus.V1.Ledger.Value

data ValidatorParams = ValidatorParams {
  prmsCurrencySymbol  :: CurrencySymbol,
  prmsPubKeyHash      :: PubKeyHash
}

PlutusTx.makeLift ''ValidatorParams
PlutusTx.unstableMakeIsData ''ValidatorParams

validatorParams :: ValidatorParams
validatorParams = ValidatorParams {
  -- cat ~/cardano/testnet/keys/acc0/payment.pkh
  prmsPubKeyHash = "b16634c241055f4a8cd95e0fe903b015eed641d0590aedaf780eb6c9",
  prmsCurrencySymbol = mpsSymbol "891b79189cc2ad6175160655a0e6286036695af10d2511f77f966e5c"
  -- cat ~/cardano/mainnet/keys/acc0/payment.pkh
  --prmsPubKeyHash = "4e38086416d9c5bc6ef667b36265ff7659f93b0e217ba456d1dbf5fe",
  --prmsCurrencySymbol = mpsSymbol "3f997b68b1f491c7c2f10af4e2bf9566c5d25bd61df0343065d4fe1c"
}

-- =============================================================================
-- | MyDatum - Datum associated with script UTxOs

data MyDatum = MyDatum {
  datTokenName :: TokenName,
  datInvalidAfter :: POSIXTime
}

instance Eq MyDatum where
  a == b = datTokenName a == datTokenName b
    && datInvalidAfter a == datInvalidAfter b

PlutusTx.makeLift ''MyDatum
PlutusTx.unstableMakeIsData ''MyDatum

-- =============================================================================
-- | Validator entry point

swapContract :: ValidatorParams -> MyDatum -> Integer -> ScriptContext -> Bool
swapContract vprms dat red ctx
  | red == 0  = handleTokenSwap vprms dat ctx
  | red == 1  = handleWithdraw vprms dat ctx
  | otherwise = traceIfFalse "Invalid redeemer value" False

-- =============================================================================
-- On-Chain utility functions

getScriptAddress :: ScriptContext -> Address
getScriptAddress ctx = scriptHashAddress $ ownHash ctx

-- Get the list of other input addresses
--
getInputAddresses :: ScriptContext -> [Address]
getInputAddresses ctx = let
    info = scriptContextTxInfo ctx
    saddr   = getScriptAddress ctx
    inaddrs = [txOutAddress txo | TxInInfo{txInInfoResolved=txo} <- txInfoInputs info]
  in nub $ filter (/= saddr) inaddrs

-- True if the given PubKeyHash has signed
--
hasPubKeyHashSigned :: PubKeyHash -> ScriptContext -> Bool
hasPubKeyHashSigned pkh ctx = checkHasPubKeyHashSigned
  where
    inputPubKeyHashes :: [Maybe PubKeyHash]
    inputPubKeyHashes = map toPubKeyHash $ getInputAddresses ctx
    checkHasPubKeyHashSigned = Just pkh `elem` inputPubKeyHashes

-- Convert builtin data to MyDatum
--
fromBuiltinData :: BuiltinData -> Maybe MyDatum
fromBuiltinData dat = case PlutusTx.fromBuiltinData dat of
    Just d@MyDatum{} -> Just d
    _                -> Nothing

-- =============================================================================
-- Action Handlers

{-|
  Redeems ADA value equivalent to the provided Astor token value

  Conditions:
    #1 Token input matches script input datum
    #2 Refund + tokens + datum paid to script
    #3 Script input has not expired
-}
handleTokenSwap :: ValidatorParams -> MyDatum -> ScriptContext -> Bool
handleTokenSwap vprms dat ctx =
    traceIfFalse "Token input matches script input datum" hasMatchingTokenInput
    && traceIfFalse "Refund + tokens + datum paid to script" hasRefundPaidToScript
    && traceIfFalse "Script input has not expired" hasNotExpired
  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    txins :: [(TxOutRef, TxOut)]
    txins = [(oref,txo) | TxInInfo{txInInfoOutRef=oref,txInInfoResolved=txo} <- txInfoInputs info]
    txouts :: [TxOut]
    txouts = txInfoOutputs info

    txinHasAsset :: AssetClass -> (TxOutRef, TxOut) -> Bool
    txinHasAsset acl txin = 0 < txinAssetValueOf acl txin

    txinAssetValueOf :: AssetClass -> (TxOutRef, TxOut) -> Integer
    txinAssetValueOf acl (_,txo) = txoutAssetValueOf acl txo

    txoutHasAsset :: AssetClass -> TxOut -> Bool
    txoutHasAsset acl txo = 0 < txoutAssetValueOf acl txo

    txoutAssetValueOf :: AssetClass -> TxOut -> Integer
    txoutAssetValueOf acl txo = assetClassValueOf (txOutValue txo) acl

    txoutMyDatum :: TxOut -> Maybe MyDatum
    txoutMyDatum txo = let
        mbdh = txOutDatumHash txo
        mbdat = hashToDatum ctx mbdh
        mydat  = case mbdat of
          Just (Datum datum) -> fromBuiltinData datum
          _                  -> Nothing
      in mydat

    isOwnInput :: (TxOutRef, TxOut) -> Bool
    isOwnInput (oref,_) = case findOwnInput ctx of
      Just TxInInfo{txInInfoOutRef=ownOutRef} -> oref == ownOutRef
      _                                       -> False

    myOwnInput :: Maybe (TxOutRef, TxOut)
    myOwnInput = case filter isOwnInput txins of
         [txin] -> Just txin
         _      -> Nothing

    -- #1 Token input matches script input datum -------------------------------
    --

    -- Get the expected token asset class from the datum
    expTokenName = datTokenName dat
    expTokenSymbol = prmsCurrencySymbol vprms
    tokenAssetClass = assetClass expTokenSymbol expTokenName

    -- Filter other inputs by expected asset class and sum up the token amounts
    otherInputs = filter (not . isOwnInput) txins
    tokenInputs = filter (txinHasAsset tokenAssetClass) otherInputs
    tokenAmount = sum $ map (txinAssetValueOf tokenAssetClass) tokenInputs

    hasMatchingTokenInput :: Bool
    hasMatchingTokenInput = 0 < tokenAmount

    -- #4 Tokens must be paid to script ----------------------------------------
    --

    adaAssetClass :: AssetClass
    adaAssetClass = assetClass adaSymbol adaToken

    -- Compute the minumum script output Lovelace
    minScriptOutputLovelace = let
        tokenLovelace = tokenAmount * 1000000
        scriptInputLovelace = case myOwnInput of
          Just txin -> txinAssetValueOf adaAssetClass txin
          _         -> 0
      in scriptInputLovelace - tokenLovelace

    -- Compute the minumum script output Lovelace
    minScriptOutputTokens = let
        scriptInputTokens = case myOwnInput of
          Just txin -> txinAssetValueOf tokenAssetClass txin
          _         -> 0
      in scriptInputTokens + tokenAmount

    validScriptOutput :: TxOut -> Bool
    validScriptOutput txo = let
        scriptOutputLovelace = txoutAssetValueOf adaAssetClass txo
        scriptOutputTokens = txoutAssetValueOf tokenAssetClass txo
        hasAssetClass = txoutHasAsset tokenAssetClass txo
        isPayingToScript = txOutAddress txo == getScriptAddress ctx
        hasValidDatum = Just dat == txoutMyDatum txo
      in hasAssetClass && isPayingToScript && hasValidDatum
        && minScriptOutputLovelace <= scriptOutputLovelace
        && minScriptOutputTokens <= scriptOutputTokens

    -- Expect at least one valid output
    tokenOutputs = filter validScriptOutput txouts

    hasRefundPaidToScript :: Bool
    hasRefundPaidToScript = not $ null tokenOutputs

    -- #3 Script input has not expired -----------------------------------------
    --

    tokenInvalidAfter :: POSIXTime
    tokenInvalidAfter = datInvalidAfter dat

    txTimeRange :: POSIXTimeRange
    txTimeRange = txInfoValidRange info

    hasNotExpired :: Bool
    hasNotExpired = after tokenInvalidAfter txTimeRange

{-|
  Withdraw ADA+Tokens

  Conditions:
    - Owner has signed the Tx
-}
handleWithdraw :: ValidatorParams -> MyDatum -> ScriptContext -> Bool
handleWithdraw vprms _ ctx =
    traceIfFalse "Owner has not signed" ownerHasSigned
  where
    ownerPubKeyHash = prmsPubKeyHash vprms
    ownerHasSigned = hasPubKeyHashSigned ownerPubKeyHash ctx

-- =============================================================================
-- Script Deployment Stuff

data AstorTokenSwap
instance Scripts.ValidatorTypes AstorTokenSwap where
    type instance DatumType AstorTokenSwap = MyDatum
    type instance RedeemerType AstorTokenSwap = Integer

tokenSwapInstance :: Scripts.TypedValidator AstorTokenSwap
tokenSwapInstance = Scripts.mkTypedValidator @AstorTokenSwap
    ($$(PlutusTx.compile [|| swapContract ||]) `PlutusTx.applyCode` PlutusTx.liftCode validatorParams)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @MyDatum @Integer

{-
    As a Validator
-}

tokenSwapValidator :: Validator
tokenSwapValidator = Scripts.validatorScript tokenSwapInstance

{-
    As a Script
-}

tokenSwapScript :: Script
tokenSwapScript = unValidatorScript tokenSwapValidator

{-
    As a Short Byte String
-}

tokenSwapSBS :: SBS.ShortByteString
tokenSwapSBS =  SBS.toShort . LBS.toStrict $ serialise tokenSwapScript

{-
    As a Serialised Script
-}

tokenSwapSerialised :: PlutusScript PlutusScriptV1
tokenSwapSerialised = PlutusScriptSerialised tokenSwapSBS

-- =============================================================================
-- Other random stuff

-- E042:Error: Unsupported feature: Kind: *
-- https://github.com/input-output-hk/plutus/issues/3914

hashToDatum :: ScriptContext -> Maybe DatumHash -> Maybe Datum
hashToDatum ctx mbdh = case mbdh of
  Just dhash -> findDatum dhash $ scriptContextTxInfo ctx
  _          -> Nothing
