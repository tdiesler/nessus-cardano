

proc astorRunTests {opts} {

  logInfo [getSectionHeader "astor $opts"]

  dict set spec "--epoch" [dict create required false]
  set args [argsInit $spec $opts]
  set epoch [argsValue $args "--epoch" [getCurrentEpoch]]

  set values "2 Ada 2 Ada 10 Astor$epoch"

  astor [list --reset --epoch $epoch]
  astor [list --pay2pkh --from Owner --to Shelley --value $values]
  astor [list --pay2script --from Owner --epoch $epoch --value "100 Ada"]
  astor [list --show all]

  testUnauthorizedWithdraw $epoch

  testInvalidTokenSwaps 10 "Astor$epoch"

  testValidTokenSwap 10 "Astor$epoch"

  astor [list --show all]
}

# Provoke invalid token swaps
#
# dict set failure condition "no-token-txin"
# dict set failure condition "tokens-not-paid-to-script"
#
proc testTokenSwap {fromInfo amount tokenName failure {targetAddr ""}} {
  global POLICY_ID
  global SCRIPTS_DIR
  global MIN_TOKEN_LOVELACE
  global MIN_PLUTUS_FEES
  global DOCKER_RUNTIME
  global scriptInfo
  set epoch [getEpochFromTokenName $tokenName]
  set assetClass "$POLICY_ID.$tokenName"
  set fromName [dict get $fromInfo name]
  set fromAddr [dict get $fromInfo addr]
  set scriptAddr [dict get $scriptInfo addr]

  puts [getSectionHeader "Swap $amount $tokenName for $fromName"]
  if {$targetAddr == ""} { set targetAddr $fromAddr }

  # Filter caller utxos by asset class
  set fromUtxos [queryUtxos $fromInfo]
  set tokenUtxos [filterUtxosBySymbol $fromUtxos $assetClass]
  set txidFees [findFeesId $fromUtxos]
  set txidCollateral [findCollateralId $fromUtxos $txidFees]
  logInfo "Fees: $txidFees [dict get $fromUtxos $txidFees value]"
  logInfo "Collateral: $txidCollateral [dict get $fromUtxos $txidCollateral value]"

  # Select the caller's token UTxO
  set lvtokens 0
  set tokenTxinId ""
  foreach txid [dict keys $tokenUtxos] {
    set value [dict get $tokenUtxos $txid value]
    if {$amount == [dict get $value $assetClass]} {
      set lvtokens [dict get $value "lovelace"]
      set tokenTxinId $txid
      break
    }
  }
  if {$tokenTxinId == ""} {
    error "Cannot find caller UTxO"
  }

  # Filter script utxos by datum
  set datumHash [getDatumHash $tokenName]
  set scriptUtxos [queryUtxos $scriptInfo]
  set scriptUtxos [filterUtxosByDatum $scriptUtxos $datumHash]

  # Select the script UTxO
  set scriptTxinId ""
  set lvamount [toLovelace $amount]
  foreach txid [dict keys $scriptUtxos] {
    set value [dict get $scriptUtxos $txid value]
    if {$lvamount <= [dict get $value "lovelace"]} {
      set scriptTxinId $txid
      break
    }
  }
  if {$scriptTxinId == ""} {
    error "Cannot find script UTxO"
  }

  # Calculate the script refund
  set scriptValue [dict get $scriptUtxos $scriptTxinId value]
  set scriptInputLovelace [dict get $scriptValue "lovelace"]
  if {[dict exists $scriptValue $assetClass]} {
    set scriptInputTokens [dict get $scriptValue $assetClass]
  } else {
    set scriptInputTokens 0
  }
  set scriptRefundTokens [expr {$scriptInputTokens + $amount}]
  set scriptRefundLovelace [expr {$scriptInputLovelace - $lvamount}]
  set scriptRefundLovelace [expr max($scriptRefundLovelace, $MIN_TOKEN_LOVELACE)]

  set scriptRefundSpec "$scriptAddr+$scriptRefundLovelace"
  if {[dict get $failure condition] == "refund-not-paid-to-script"} {
    set scriptRefundSpec "$scriptAddr+$MIN_TOKEN_LOVELACE"
  }
  if {[dict get $failure condition] == "too-little-refund-paid-to-script"} {
    set scriptRefundSpec "$scriptAddr+[expr {$scriptRefundLovelace - 1}]"
  }
  if {[dict get $failure condition] != "tokens-not-paid-to-script"} {
    append scriptRefundSpec "+$scriptRefundTokens $assetClass"
  }
  if {[dict get $failure condition] == "too-few-tokens-paid-to-script"} {
    set scriptRefundSpec "$scriptAddr+$scriptRefundLovelace"
    append scriptRefundSpec "+[expr {$scriptRefundTokens - 1}] $assetClass"
  }

  if {[dict get $failure condition] == "wrong-refund-datum"} {
    set datumHash [getDatumHash "Astor[expr {$epoch + 1}]"]
  }

  # Calculate the invalid after slot
  set slotDelta 300
  if {[dict get $failure condition] == "invalid-hereafter"} {
    incr slotDelta 1000
  }
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
  if {[dict get $failure condition] != "no-token-txin"} {
    lappend args "--tx-in" $tokenTxinId
  }
  lappend args "--tx-in" $scriptTxinId
  lappend args "--tx-in-script-file" "/var/cardano/local/$SCRIPTS_DIR/swaptokens.plutus"
  lappend args "--tx-in-datum-file" "/var/cardano/local/scratch/script-datum$epoch.json"
  lappend args "--tx-in-redeemer-value" 0
  lappend args "--tx-in-collateral" $txidCollateral
  lappend args "--tx-out" $scriptRefundSpec
  lappend args "--tx-out-datum-hash" $datumHash
  lappend args "--tx-out" "$targetAddr+[expr {$lvamount + $lvtokens - $MIN_PLUTUS_FEES}]"
  lappend args "--change-address" $fromAddr
  lappend args "--invalid-hereafter" $targetSlot
  lappend args "--protocol-params-file" [getProtocolConfig]
  lappend args "--out-file" "/var/cardano/local/scratch/tx.raw"
  puts [cardano-cli $args]

  if {[dict get $failure condition] == "valid-token-swap"} {
    cliTxSign $fromInfo
    cliTxSubmit $fromInfo $txidFees
  } else {
    error "Expected to fail"
  }
}

proc testUnauthorizedWithdraw {epoch} {
  set fromInfo [getAddrInfo 1]
  catch {
    set msg "Unauthorized withdraw"
    puts [getSectionHeader $msg]
    scriptWithdraw $fromInfo $epoch
    error $msg
  }
}

proc testInvalidTokenSwaps {amount tokenName} {
  set fromInfo [getAddrInfo 1]

  catch {
    set msg "Caller sends no tokens"
    puts [getSectionHeader $msg]
    dict set failure condition "no-token-txin"
    testTokenSwap $fromInfo $amount $tokenName $failure
    error $msg
  }
  catch {
    set msg "Tokens not paid to script"
    puts [getSectionHeader $msg]
    dict set failure condition "tokens-not-paid-to-script"
    testTokenSwap $fromInfo $amount $tokenName $failure
    error $msg
  }
  catch {
    set msg "Ada refund not paid to script"
    puts [getSectionHeader $msg]
    dict set failure condition "refund-not-paid-to-script"
    testTokenSwap $fromInfo $amount $tokenName $failure
    error $msg
  }
  catch {
    set msg "Too few tokens paid to script"
    puts [getSectionHeader $msg]
    dict set failure condition "too-few-tokens-paid-to-script"
    testTokenSwap $fromInfo $amount $tokenName $failure
    error $msg
  }
  catch {
    set msg "Too little refund paid to script"
    puts [getSectionHeader $msg]
    dict set failure condition "too-little-refund-paid-to-script"
    testTokenSwap $fromInfo $amount $tokenName $failure
    error $msg
  }
  catch {
    set msg "Refund with wrong datum"
    puts [getSectionHeader $msg]
    dict set failure condition "wrong-refund-datum"
    testTokenSwap $fromInfo $amount $tokenName $failure
    error $msg
  }
  catch {
    set msg "Invalid hereafter"
    puts [getSectionHeader $msg]
    dict set failure condition "invalid-hereafter"
    testTokenSwap $fromInfo $amount $tokenName $failure
    error $msg
  }
}

proc testValidTokenSwap {amount tokenName} {
  set fromInfo [getAddrInfo 1]
  set msg "Valid token swap"
  puts [getSectionHeader $msg]
  dict set failure condition "valid-token-swap"
  testTokenSwap $fromInfo $amount $tokenName $failure
}
