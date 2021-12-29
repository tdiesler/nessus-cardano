
dict set addrMap 0 name "Owner"
if {[file exists "$KEYS_DIR/acc0/payment.addr"]} {
  dict set addrMap 0 addr [fileRead "$KEYS_DIR/acc0/payment.addr"]
  dict set addrMap 0 skey "$BLOCKFROST_NETWORK/keys/acc0/payment.skey"
}

dict set addrMap 1 name "Shelley"
if {[file exists "$KEYS_DIR/acc1/payment.addr"]} {
  dict set addrMap 1 addr [fileRead "$KEYS_DIR/acc1/payment.addr"]
  dict set addrMap 1 skey "$BLOCKFROST_NETWORK/keys/acc1/payment.skey"
}

dict set addrMap 2 name "Mary"
if {[file exists "$KEYS_DIR/acc2/payment.addr"]} {
  dict set addrMap 2 addr [fileRead "$KEYS_DIR/acc2/payment.addr"]
  dict set addrMap 2 skey "$BLOCKFROST_NETWORK/keys/acc2/payment.skey"
}

dict set addrMap 3 name "Percy"
if {[file exists "$KEYS_DIR/acc3/payment.addr"]} {
  dict set addrMap 3 addr [fileRead "$KEYS_DIR/acc3/payment.addr"]
  dict set addrMap 3 skey "$BLOCKFROST_NETWORK/keys/acc3/payment.skey"
}

dict set addrMap 4 name "Script"
dict set addrMap 4 addr $SCRIPT_ADDR

set ownerInfo   [getAddrInfo 0]
set shelleyInfo [getAddrInfo 1]
set maryInfo    [getAddrInfo 2]
set percyInfo   [getAddrInfo 3]
set scriptInfo  [getAddrInfo 4]

proc payAllToPubKeyHash {fromInfo toInfo} {
  set fromName [dict get $fromInfo name]

  logInfo "Withdraw all assets from $fromName"

  set utxos [queryUtxos $fromInfo false]
  set txids [dict keys $utxos]

  if {[llength $txids] > 0} {
    dict set txoutAmounts "lovelace" 0
    foreach txid $txids {
      set value [dict get $utxos $txid value]
      foreach symbol [dict keys $value] {
        set amount [dict get $value $symbol]
        if {![dict exists $txoutAmounts $symbol]} {
          dict set txoutAmounts $symbol 0
        }
        set symamt [dict get $txoutAmounts $symbol]
        dict set txoutAmounts $symbol [expr {$symamt + $amount}]
      }
    }
    foreach symbol [dict keys $txoutAmounts] {
      set amount [dict get $txoutAmounts $symbol]
      dict set txoutSpecs 0 txoutAddress $toInfo
      dict set txoutSpecs 0 txoutValue $symbol $amount
    }
    payToPubKeyHash $fromInfo $txoutSpecs $toInfo
  }
}

# Pay to one or more PubKeyHash
#
# dict set txoutSpecs idx txoutAddress {name value, addr value}
# dict set txoutSpecs idx txoutValue symbol amount
# dict set txoutSpecs idx txoutValue symbol amount
#
proc payToPubKeyHash {fromInfo txoutSpecs {changeInfo 0}} {
  global TRY_RUN
  global MAX_INTEGER
  global MIN_TOKEN_LOVELACE
  set fromName [dict get $fromInfo name]
  set fromAddr [dict get $fromInfo addr]

  # Default the change address to the from address
  if {$changeInfo == 0} { set changeInfo $fromInfo }
  set changeAddr [dict get $changeInfo addr]

  set utxos [queryUtxos $fromInfo]

  set txoutAmounts [getTxoutAmounts $fromInfo $txoutSpecs true]
  set txinSpecs [getTxinSpecs $fromInfo $utxos $txoutSpecs]

  logDebug "txoutSpecs: $txoutSpecs"
  logDebug "txinSpecs: $txinSpecs"

  # Aggregate the selected TxIds
  set selectedTxinIds [list]
  foreach symbol [dict keys $txinSpecs] {
    set selectedTxinIds [concat $selectedTxinIds [dict get $txinSpecs $symbol txids]]
  }

  # Check if we pay all available lovelace
  set totalLovelaceOut [dict get $txoutAmounts "lovelace"]
  set totalLovelaceIn [dict get $txinSpecs "lovelace" amount]
  set totalLovelaceChange [expr {$totalLovelaceIn - $totalLovelaceOut}]
  set payAllLovelace [expr {$totalLovelaceChange} == 0]
  logInfo "Lovelace: $totalLovelaceIn - $totalLovelaceOut => $totalLovelaceChange"
  if {$totalLovelaceChange < 0} {
    error "The transaction does not balance in its use of ada"
  }

  # Append the token refund
  foreach symbol [dict keys $txoutAmounts] {
    if {$symbol != "lovelace"} {
      set txinAmount [dict get $txinSpecs $symbol amount]
      set txoutAmount [dict get $txoutAmounts $symbol]
      set refundAmount [expr {$txinAmount - $txoutAmount}]
      if {0 < $refundAmount} {
        set key "[getTokenName $symbol].Refund"
        dict set txoutSpecs $key txoutAddress $fromInfo
        dict set txoutSpecs $key txoutValue "lovelace" $MIN_TOKEN_LOVELACE
        dict set txoutSpecs $key txoutValue $symbol $refundAmount
      }
    }
  }

  # Build the Tx
  logInfo "Build payment transaction"
  set args [networkAwareCmd [list "transaction" "build"]]
  lappend args "--alonzo-era"
  foreach txid $selectedTxinIds {
    lappend args "--tx-in" $txid
  }
  foreach idx [dict key $txoutSpecs] {
    set subdct [dict get $txoutSpecs $idx]
    set txoutAddr [dict get $subdct txoutAddress]
    set txoutValue [dict get $subdct txoutValue]
    set txoutSpec [dict get $txoutAddr addr]
    set symbols [dict keys $txoutValue]
    set slength [llength $symbols]
    set hasTokens [expr {$slength > 1 || [lindex $symbols 0] != "lovelace"}]
    if {$hasTokens} {
      set assetClass [lindex $symbols [expr {$slength - 1}]]
      set assetClassHex [assetClassToHex $assetClass]
      set tokenAmount [dict get $txoutValue $symbol]
      lappend args "--tx-out" "$txoutSpec+$MIN_TOKEN_LOVELACE+$tokenAmount $assetClassHex"
    } else { if {!$payAllLovelace} {
      set lvamount [dict get $txoutValue "lovelace"]
      lappend args "--tx-out" "$txoutSpec+$lvamount"
    }}
  }
  lappend args "--change-address" $changeAddr
  lappend args "--out-file" "/var/cardano/local/scratch/tx.raw"
  cardano-cli $args

  # Sign the Tx
  cliTxSign $fromInfo

  # Submit the Tx
  if {!$TRY_RUN} {
    set firstTxid [lindex $selectedTxinIds 0]
    return [cliTxSubmit $fromInfo $firstTxid]
  }
}

proc payToScript {fromInfo lvamount tokenName ttl} {
  global TRY_RUN
  global SCRATCH_DIR
  global SCRIPT_ADDR
  global TOKEN_TTL_EPOCHS
  global scriptInfo
  set fromName [dict get $fromInfo name]
  set fromAddr [dict get $fromInfo addr]

  logInfo "Pay $lvamount lovelace from $fromName to Script"

  if {$ttl == 0} {
    set epoch [getEpochFromTokenName $tokenName]
    set ttl "e[expr {$epoch + $TOKEN_TTL_EPOCHS}]"
  }

  set utxos [queryUtxos $fromInfo]

  dict set txoutSpecs 0 txoutAddress $scriptInfo
  dict set txoutSpecs 0 txoutValue "lovelace" $lvamount

  set txoutAmounts [getTxoutAmounts $fromInfo $txoutSpecs true]
  set txinSpecs [getTxinSpecs $fromInfo $utxos $txoutSpecs]
  logDebug "TxoutSpecs: $txoutSpecs"
  logDebug "TxinSpecs: $txinSpecs"

  # Aggregate the selected TxIds
  set selectedTxinIds [list]
  foreach symbol [dict keys $txinSpecs] {
    set selectedTxinIds [concat $selectedTxinIds [dict get $txinSpecs $symbol txids]]
  }

  # Check if we pay all available lovelace
  set totalLovelaceOut [dict get $txoutAmounts "lovelace"]
  set totalLovelaceIn [dict get $txinSpecs "lovelace" amount]
  set totalLovelaceChange [expr {$totalLovelaceIn - $totalLovelaceOut}]
  set payAllLovelace [expr {$totalLovelaceChange} == 0]
  logInfo "Lovelace: $totalLovelaceIn - $totalLovelaceOut => $totalLovelaceChange"
  if {$totalLovelaceChange < 0} {
    error "The transaction does not balance in its use of ada"
  }

  # Append the token refund
  foreach symbol [dict keys $txoutAmounts] {
    if {$symbol != "lovelace"} {
      set txinAmount [dict get $txinSpecs $symbol amount]
      set txoutAmount [dict get $txoutAmounts $symbol]
      set refundAmount [expr {$txinAmount - $txoutAmount}]
      if {0 < $refundAmount} {
        set key "[getTokenName $symbol].Refund"
        dict set txoutSpecs $key txoutAddress $fromInfo
        dict set txoutSpecs $key txoutValue "lovelace" $MIN_TOKEN_LOVELACE
        dict set txoutSpecs $key txoutValue $symbol $refundAmount
      }
    }
  }

  set invalidAfterTime [getInvalidAfterTime $ttl]
  set datumHash [writeDatumFile $tokenName $invalidAfterTime]

  # Build the transaction
  logInfo "Build pay to script transaction"
  set args [networkAwareCmd [list "transaction" "build"]]
  lappend args "--alonzo-era"
  foreach txid $selectedTxinIds {
    lappend args "--tx-in" $txid
  }
  lappend args "--tx-out" "$SCRIPT_ADDR+$lvamount"
  lappend args "--tx-out-datum-hash" $datumHash
  lappend args "--change-address" $fromAddr
  lappend args "--protocol-params-file" [getProtocolConfig]
  lappend args "--out-file" "/var/cardano/local/scratch/tx.raw"
  cardano-cli $args

  # Sign the Tx
  cliTxSign $fromInfo

  # Submit the Tx
  if {!$TRY_RUN} {
    set firstTxid [lindex $selectedTxinIds 0]
    return [cliTxSubmit $fromInfo $firstTxid]
  }
}

# Find UTxOs for the given address
#
# txid#idx 'value' (currency amount)+ 'datum' datum
#
proc queryUtxos {fromInfo {show true}} {
  global addrMap
  set fromName [dict get $fromInfo name]
  set fromAddr [dict get $fromInfo addr]

  if {$show} {
    logInfo "Query UTxO for $fromName"
  }

  set args [networkAwareCmd [list "query" "utxo"]]
  lappend args "--address" $fromAddr
  set rawout [cardano-cli $args false]

  # Remove the header lines
  set lines [split $rawout "\n"]
  set lines [lrange $lines 2 end]

  # Create the empty result dict
  set unsortedUtxos [dict create]
  set sortedUtxos [dict create]

  # Transform UTxOs into a dictionary
  #
  # txid#idx 'value' (currency1 amount1)+ 'datum' datum
  #
  foreach line $lines {
    regexp {([a-f0-9]+) +(\d+) +(.+)} $line match tok1 tok2 tok3
    set txid "$tok1#$tok2"
    set tail [splitTrim $tok3 '+']
    set taillen [llength $tail]
    set valdict [dict create]
    foreach val [lrange $tail 0 [expr {$taillen - 2}]] {
      set aux [split $val]
      set symbol [lindex $aux 1]
      set amount [lindex $aux 0]
      if {$symbol == "lovelace"} {
        dict set valdict $symbol $amount
      } else {
        set assetClass [assetClassFromHex $symbol]
        dict set valdict $assetClass $amount
      }
    }
    set lastidx [expr {$taillen - 1}]
    set datumWord [lindex $tail $lastidx]
    set datumToks [splitTrim $datumWord]
    set datumlen [llength $datumToks]
    set datum [lindex $datumToks [expr {$datumlen - 1}]]
    set datum [string map {"\"" ""} $datum]
    # Ignore script utxo with no datum
    if {$fromName != "Script" || $datum != "TxOutDatumNone"} {
      dict set unsortedUtxos $txid value $valdict
      dict set unsortedUtxos $txid datum $datum
    }
  }

  # Sort the utxos by lovelace value
  set unsortedIds [dict keys $unsortedUtxos]
  set unsortedAmounts [list]
  set sortedIds [list]

  foreach txid $unsortedIds {
    lappend unsortedAmounts [dict get $unsortedUtxos $txid value lovelace]
  }

  while {0 < [llength $unsortedIds]} {
    set low [lindex [lsort -integer $unsortedAmounts] 0]
    set idx [lsearch $unsortedAmounts $low]
    lappend sortedIds [lindex $unsortedIds $idx]
    set unsortedAmounts [ldelete $unsortedAmounts $idx]
    set unsortedIds [ldelete $unsortedIds $idx]
  }

  set i 0
  while {$i < [llength $sortedIds]} {
    set txid [lindex $sortedIds $i]
    set value [dict get $unsortedUtxos $txid value]
    set datum [dict get $unsortedUtxos $txid datum]
    dict set sortedUtxos $txid value $value
    dict set sortedUtxos $txid datum $datum
    incr i
  }

  # Show result utxos
  if {$show} {
    showUtxos $sortedUtxos
  }

  return $sortedUtxos
}

proc sendTokensToPubKeyHash {fromInfo toInfo tokenAmounts epoch} {
  global POLICY_ID
  global MIN_TOKEN_LOVELACE
  set tokenName "Astor$epoch"
  set assetClass "$POLICY_ID.$tokenName"
  set toName [dict get $toInfo name]

  logInfo "Send tokens to $toName"

  for {set i 0} {$i < [llength $tokenAmounts]} {incr i} {
    set amount [lindex $tokenAmounts $i]
    dict set txoutSpecs $i txoutAddress $toInfo
    dict set txoutSpecs $i txoutValue "lovelace" $MIN_TOKEN_LOVELACE
    dict set txoutSpecs $i txoutValue $assetClass $amount
  }
  payToPubKeyHash $fromInfo $txoutSpecs
}

proc showAllWallets {} {
  global addrMap
  foreach id [dict keys $addrMap] {
    if [dict exists $addrMap $id "addr"] {
      showWallet [dict get $addrMap $id]
    }
  }
}

proc showUtxos {utxos} {
  foreach txid [dict keys $utxos] {
    set subdct [dict get $utxos $txid]
    puts "  $txid $subdct"
  }
}

proc showWallet {addrInfo} {
  puts ""
  queryUtxos $addrInfo
}
