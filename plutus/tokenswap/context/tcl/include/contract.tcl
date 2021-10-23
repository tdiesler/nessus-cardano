
proc scriptMintTokens {fromInfo amount tokenName} {
  global POLICY_ID
  global SCRIPTS_DIR
  global MIN_SEND_AMOUNT
  set assetClass "$POLICY_ID.$tokenName"
  set fromName [dict get $fromInfo name]
  set fromAddr [dict get $fromInfo addr]

  logInfo "Mint $amount $tokenName tokens for $fromName"

  set fromUtxos [queryUtxos $fromInfo]

  # Find fees and collateral
  set txidFees [findFeesId $fromUtxos [toLovelace 4]]
  set txidCollateral [findCollateralId $fromUtxos $txidFees]
  logInfo "Fees: $txidFees [dict get $fromUtxos $txidFees value]"
  logInfo "Collateral: $txidCollateral [dict get $fromUtxos $txidCollateral value]"

  # Build the Tx
  logInfo "Build token mint transaction"
  set args [networkAwareCmd [list "transaction" "build"]]
  lappend args "--alonzo-era"
  lappend args "--tx-in" $txidFees
  lappend args "--tx-in-collateral" $txidCollateral
  lappend args "--mint" "$amount $assetClass"
  lappend args "--mint-script-file" "/var/cardano/local/$SCRIPTS_DIR/minttokens.plutus"
  lappend args "--mint-redeemer-value" $amount
  lappend args "--tx-out" "$fromAddr+$MIN_SEND_AMOUNT+$amount $assetClass"
  lappend args "--change-address" $fromAddr
  lappend args "--protocol-params-file" [getProtocolConfig]
  lappend args "--out-file" "/var/cardano/local/scratch/tx.raw"
  cardano-cli $args

  # Sign the Tx
  cliTxSign $fromInfo

  # Submit the Tx
  cliTxSubmit $fromInfo $txidFees
}

proc scriptBurnTokens {fromInfo tokenName} {
  global POLICY_ID
  global SCRIPTS_DIR
  global MIN_COLLATERAL
  set assetClass "$POLICY_ID.$tokenName"
  set fromName [dict get $fromInfo name]
  set fromAddr [dict get $fromInfo addr]

  logInfo "Burn $tokenName tokens for $fromName"

  set fromUtxos [queryUtxos $fromInfo]

  # Select utxos
  set burnAmount 0
  set selectedIds [list]
  foreach txid [dict keys $fromUtxos] {
    set subdct [dict get $fromUtxos $txid value]
    set symbols [dict keys $subdct]
    if {[lsearch $symbols $assetClass] > 0} {
      set amount [dict get $subdct $assetClass]
      lappend selectedIds $txid
      incr burnAmount $amount
    }
  }

  if { $burnAmount <= 0 } {
    logWarn "Cannot find tokens to burn"
    return false
  }

  # Find fees and collateral
  set firstTxid [lindex $selectedIds 0]
  set txidCollateral [findCollateralId $fromUtxos]
  logInfo "Collateral: $txidCollateral [dict get $fromUtxos $txidCollateral value]"

  # Build the Tx
  logInfo "Build token burn transaction"
  set args [networkAwareCmd [list "transaction" "build"]]
  lappend args "--alonzo-era"
  foreach txid $selectedIds {
    lappend args "--tx-in" $txid
  }
  lappend args "--tx-in-collateral" $txidCollateral
  lappend args "--mint" "-$burnAmount $assetClass"
  lappend args "--mint-script-file" "/var/cardano/local/$SCRIPTS_DIR/minttokens.plutus"
  lappend args "--mint-redeemer-value" "-$burnAmount"
  lappend args "--change-address" $fromAddr
  lappend args "--protocol-params-file" [getProtocolConfig]
  lappend args "--out-file" "/var/cardano/local/scratch/tx.raw"
  cardano-cli $args

  # Sign the Tx
  cliTxSign $fromInfo

  # Submit the Tx
  set firstTxid [lindex $selectedIds 0]
  cliTxSubmit $fromInfo $firstTxid
}

# Invoke the swap method on the smart contract
#
# astor --script swap --from Percy --to Shelley --value '10 Astor164'
# --fee-policy [caller|proxy]
proc scriptSwapTokens {fromInfo amount tokenName {feePolicy "caller"} {targetAddr ""}} {
  global POLICY_ID
  global SCRIPTS_DIR
  global MIN_SEND_AMOUNT
  global DOCKER_RUNTIME
  global scriptInfo
  set epoch [getEpochFromTokenName $tokenName]
  set assetClass "$POLICY_ID.$tokenName"
  set fromName [dict get $fromInfo name]
  set fromAddr [dict get $fromInfo addr]
  set scriptAddr [dict get $scriptInfo addr]

  logInfo "Swap $amount $tokenName from $fromName"
  if {$targetAddr == ""} { set targetAddr $fromAddr }

  # Filter caller utxos by asset class
  set fromUtxos [queryUtxos $fromInfo]
  set tokenUtxos [filterUtxosBySymbol $fromUtxos $assetClass]
  set txidFees [findFeesId $fromUtxos]
  set txidCollateral [findCollateralId $fromUtxos $txidFees]
  logInfo "Fees: $txidFees [dict get $fromUtxos $txidFees value]"
  logInfo "Collateral: $txidCollateral [dict get $fromUtxos $txidCollateral value]"

  # Select the caller's token UTxO
  set selectedTokenTxinId ""
  foreach txid [dict keys $tokenUtxos] {
    set value [dict get $tokenUtxos $txid value]
    if {$amount == [dict get $value $assetClass]} {
      set lvamount [dict get $value "lovelace"]
      set lvextra [expr {$feePolicy == "proxy" ? $lvamount : 0}]
      set selectedTokenTxinId $txid
      break
    }
  }
  if {$selectedTokenTxinId == ""} {
    error "Cannot find caller UTxO"
  }

  # Filter script utxos by datum
  set datumHash [getDatumHash $tokenName]
  set scriptUtxos [queryUtxos $scriptInfo]
  set scriptUtxos [filterUtxosByDatum $scriptUtxos $datumHash]

  # Select the script UTxO
  set selectedScriptTxinId ""
  set lvamount [toLovelace $amount]
  foreach txid [dict keys $scriptUtxos] {
    set value [dict get $scriptUtxos $txid value]
    if {$lvamount <= [dict get $value "lovelace"]} {
      set selectedScriptTxinId $txid
      break
    }
  }
  if {$selectedScriptTxinId == ""} {
    error "Cannot find script UTxO"
  }

  # Calculate the script refund
  set scriptValue [dict get $scriptUtxos $selectedScriptTxinId value]
  set scriptInputLovelace [dict get $scriptValue "lovelace"]
  if {[dict exists $scriptValue $assetClass]} {
    set scriptInputTokens [dict get $scriptValue $assetClass]
  } else {
    set scriptInputTokens 0
  }
  set scriptRefundTokens [expr {$scriptInputTokens + $amount}]
  set scriptRefundLovelace [expr {$scriptInputLovelace - $lvamount}]
  set scriptRefundLovelace [expr max($scriptRefundLovelace, $MIN_SEND_AMOUNT)]

  # Calculate the invalid after slot
  set slotDelta 300
  set bounds [getEpochBoundaries]
  set currentSlot [dict get $bounds currentSlot]
  set currentTime [dict get $bounds currentTime]
  set targetSlot [expr {$currentSlot + $slotDelta}]
  set targetTime [expr {$currentTime + $slotDelta}]

  set timestr [clock format $targetTime -format {%d-%m-%Y %H:%M:%S %Z}]
  logInfo "InvalidAfter: $targetSlot => $timestr"

  # Sanity check external files that must be available
  set scriptFile "/var/cardano/local/$SCRIPTS_DIR/swaptokens.plutus"
  set datumFile "/var/cardano/local/scratch/script-datum$epoch.json"
  set protocolFile "/var/cardano/local/scratch/protocol.json"

  if {$DOCKER_RUNTIME} {
    if {![file exists $protocolFile]} { error "Protocol params file does not exist: $protocolFile"}
    if {![file exists $scriptFile]} { error "Script file does not exist: $scriptFile"}
    if {![file exists $datumFile]} { error "Datum file does not exist: $datumFile"}
  }

  # Build the transaction
  logInfo "Build token swap transaction"
  set args [networkAwareCmd [list "transaction" "build"]]
  lappend args "--alonzo-era"
  lappend args "--tx-in" $txidFees
  lappend args "--tx-in" $selectedTokenTxinId
  lappend args "--tx-in" $selectedScriptTxinId
  lappend args "--tx-in-script-file" "/var/cardano/local/$SCRIPTS_DIR/swaptokens.plutus"
  lappend args "--tx-in-datum-file" "/var/cardano/local/scratch/script-datum$epoch.json"
  lappend args "--tx-in-redeemer-value" 0
  lappend args "--tx-in-collateral" $txidCollateral
  lappend args "--tx-out" "$scriptAddr+$scriptRefundLovelace+$scriptRefundTokens $assetClass"
  lappend args "--tx-out-datum-hash" $datumHash
  lappend args "--tx-out" "$targetAddr+[expr {$lvamount + $lvextra}]"
  lappend args "--invalid-hereafter" $targetSlot
  lappend args "--change-address" $fromAddr
  lappend args "--protocol-params-file" [getProtocolConfig]
  lappend args "--out-file" "/var/cardano/local/scratch/tx.raw"
  cardano-cli $args

  # Sign the Tx
  cliTxSign $fromInfo

  # Submit the Tx
  cliTxSubmit $fromInfo $txidFees
}

proc scriptWithdraw {fromInfo epoch} {
  global SCRIPTS_DIR
  global MIN_SEND_AMOUNT
  global scriptInfo
  set tokenName "Astor$epoch"
  set fromName [dict get $fromInfo name]
  set fromAddr [dict get $fromInfo addr]

  logInfo "Withdraw epoch $epoch from Script to $fromName"

  # Filter script utxos by datum
  set datumHash [getDatumHash $tokenName]
  set scriptUtxos [queryUtxos $scriptInfo]
  set scriptUtxos [filterUtxosByDatum $scriptUtxos $datumHash]

  if { [dict size $scriptUtxos] < 1 } {
    logWarn "Nothing to withdraw"
    return false
  }

  # Find fees and collateral
  set fromUtxos [queryUtxos $fromInfo]
  set txidFees [findFeesId $fromUtxos]
  set txidCollateral [findCollateralId $fromUtxos $txidFees]
  logInfo "Fees: $txidFees [dict get $fromUtxos $txidFees value]"
  logInfo "Collateral: $txidCollateral [dict get $fromUtxos $txidCollateral value]"

  # Build the transaction
  logInfo "Build withdraw transaction"
  set args [networkAwareCmd [list "transaction" "build"]]
  lappend args "--alonzo-era"
  lappend args "--tx-in" $txidFees
  foreach txid [dict keys $scriptUtxos] {
    set value [dict get $scriptUtxos $txid value]
    set symbols [dict keys $value]
    set isAdaOnly [expr {[llength $symbols] == 1}]
    lappend args "--tx-in" $txid
    lappend args "--tx-in-script-file" "/var/cardano/local/$SCRIPTS_DIR/swaptokens.plutus"
    lappend args "--tx-in-datum-file" "/var/cardano/local/scratch/script-datum$epoch.json"
    lappend args "--tx-in-redeemer-value" 1
    if {$isAdaOnly} {
      set lvamount [dict get $value "lovelace"]
      lappend args "--tx-out" "$fromAddr+$lvamount"
    } else {
      set assetClass [lindex $symbols 1]
      set amount [dict get $value $assetClass]
      lappend args "--tx-out" "$fromAddr+$MIN_SEND_AMOUNT+$amount $assetClass"
    }
  }
  lappend args "--tx-in-collateral" $txidCollateral
  lappend args "--change-address" $fromAddr
  lappend args "--protocol-params-file" [getProtocolConfig]
  lappend args "--out-file" "/var/cardano/local/scratch/tx.raw"
  cardano-cli $args

  # Sign the Tx
  cliTxSign $fromInfo

  # Submit the Tx
  cliTxSubmit $fromInfo $txidFees
}
