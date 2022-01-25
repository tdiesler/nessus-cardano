
proc astor {opts} {
  global TRY_RUN

  if {[llength $opts] == 0} { astorHelp; return }
  logInfo [getSectionHeader "astor $opts"]

  # Set the global try run option
  dict set spec "--try-run" [dict create required false]
  set args [argsInit $spec $opts]
  set TRY_RUN [argsValueExists $args "--try-run"]

  set cmd [lindex $opts 0]
  switch $cmd {
    --reset       { astorReset $opts }
    --pay2pkh     { astorPay2PKH $opts }
    --pay2script  { astorPay2Script $opts }
    --script      { astorScript $opts }
    --show        { astorShow $opts }
    default       { logError "Invalid command: $cmd"; astorHelp }
  }
}

proc astorPay2PKH {opts} {
  global POLICY_ID
  dict set spec "--from" [dict create required true]
  dict set spec "--to" [dict create required true]
  dict set spec "--value" [dict create required true]
  set args [argsInit $spec $opts]
  set fromName [argsValue $args "--from"]
  set fromInfo [getAddrInfoByName $fromName]
  set toSpec [argsValue $args "--to"]
  if {[string match "addr*" $toSpec]} {
    dict set toInfo name "PKHAddr"
    dict set toInfo addr $toSpec
  } else {
    set toInfo [getAddrInfoByName $toSpec]
  }
  set value [argsValue $args "--value"]
  if [string match -nocase "all" $value] {
    payAllToPubKeyHash $fromInfo $toInfo
  } else {
    set toks [splitTrim $value]
    if {[llength $toks] % 2 != 0} { error "Invalid value spec: $value" }
    for {set i 0} {$i < [llength $toks]} {incr i 2} {
      set amount [lindex $toks $i]
      set symbol [lindex $toks [expr {$i + 1}]]
      if [string match -nocase "Ada" $symbol] {
        set amount [toLovelace $amount]
        set symbol "lovelace"
      }
      if [string match -nocase "Astor*" $symbol] {
        set symbol "$POLICY_ID.$symbol"
      }
      dict set txoutSpecs $i txoutAddress $toInfo
      dict set txoutSpecs $i txoutValue $symbol $amount
    }
    payToPubKeyHash $fromInfo $txoutSpecs
  }
}

proc astorPay2Script {opts} {
  global MIN_TOKEN_LOVELACE
  dict set spec "--epoch" [dict create required false]
  dict set spec "--from" [dict create required true]
  dict set spec "--value" [dict create required true]
  dict set spec "--ttl" [dict create required false]
  set args [argsInit $spec $opts]
  set fromName [argsValue $args "--from"]
  set fromInfo [getAddrInfoByName $fromName]
  set epoch [argsValue $args "--epoch" [getCurrentEpoch]]
  set tokenName "Astor$epoch"
  set ttl [argsValue $args "--ttl" 0]
  set value [argsValue $args "--value"]
  set toks [splitTrim $value]
  if {[llength $toks] != 2} { error "Invalid value spec: $value" }
  set amount [lindex $toks 0]
  set symbol [lindex $toks 1]
  if [string match -nocase "Ada" $symbol] {
    set amount [toLovelace $amount]
    set symbol "lovelace"
  }
  if {![string match -nocase "lovelace" $symbol]} { error "Invalid value spec: $value" }
  set lvamount [expr {$amount + $MIN_TOKEN_LOVELACE}]
  payToScript $fromInfo $lvamount $tokenName $ttl
}

proc astorScript {opts} {
  global POLICY_ID
  set cmd [lindex $opts 1]
  switch -nocase $cmd {
    "burn" {
      dict set spec "--value" [dict create required true]
      dict set spec "--from" [dict create required false]
      set args [argsInit $spec $opts]
      set value [splitTrim [argsValue $args "--value"]]
      set amount [lindex $value 0]
      set tokenName [lindex $value 1]
      set fromName [argsValue $args "--from" Owner]
      set fromInfo [getAddrInfoByName $fromName]
      scriptBurnTokens $fromInfo $amount $tokenName
    }
    "mint" {
      dict set spec "--value" [dict create required true]
      dict set spec "--from" [dict create required false]
      set args [argsInit $spec $opts]
      set value [splitTrim [argsValue $args "--value"]]
      set amount [lindex $value 0]
      set tokenName [lindex $value 1]
      set fromName [argsValue $args "--from" Owner]
      set fromInfo [getAddrInfoByName $fromName]
      scriptMintTokens $fromInfo $amount $tokenName
    }
    "swap" {
      dict set spec "--from" [dict create required true]
      dict set spec "--value" [dict create required true]
      dict set spec "--to-addr" [dict create required false]
      dict set spec "--to" [dict create required false]
      set args [argsInit $spec $opts]
      set value [argsValue $args "--value"]
      set fromName [argsValue $args "--from"]
      set fromInfo [getAddrInfoByName $fromName]
      set targetAddr [argsValue $args "--to-addr" ""]
      set toName [argsValue $args "--to" ""]
      if {$targetAddr == "" && $toName != ""} {
        set toInfo [getAddrInfoByName $toName]
        set targetAddr [dict get $toInfo addr]
      }
      set amount [lindex $value 0]
      set tokenName [lindex $value 1]
      scriptSwapTokens $fromInfo $amount $tokenName $targetAddr
    }
    "withdraw" {
      dict set spec "--epoch" [dict create required false]
      dict set spec "--from" [dict create required false]
      set args [argsInit $spec $opts]
      set epoch [argsValue $args "--epoch" [getCurrentEpoch]]
      set fromName [argsValue $args "--from" Owner]
      set fromInfo [getAddrInfoByName $fromName]
      scriptWithdraw $fromInfo $epoch
    }
    default { error "Invalid script args: $opts" }
  }
}

proc astorShow {opts} {
  foreach name [lrange $opts 1 end] {
    switch -nocase $name {
      "all"   {
        showAllWallets
      }
      default {
        if {[string match "addr*" $name]} {
          dict set addrInfo name "PKHAddr"
          dict set addrInfo addr $name
        } else {
          set addrInfo [getAddrInfoByName $name]
        }
        showWallet $addrInfo
      }
    }
  }
}

proc assetClassFromHex {assetClass} {
  set toks [split $assetClass "."]
  set currencySymbol [lindex $toks 0]
  set tokenName [lindex $toks 1]
  return "${currencySymbol}.[stringFromHEX $tokenName]"
}

proc assetClassToHex {assetClass} {
  set toks [split $assetClass "."]
  set currencySymbol [lindex $toks 0]
  set tokenName [lindex $toks 1]
  return "${currencySymbol}.[stringToHEX $tokenName]"
}

proc cardano-cli {arglst {show true}} {
  global CARDANO_DIR
  global SCRATCH_DIR
  if {[file exists "/usr/local/bin/cardano-cli"]} {
    lappend cmd cardano-cli
  } else {
    lappend cmd docker run -it --rm
    lappend cmd -v "$CARDANO_DIR:/var/cardano/local"
    lappend cmd -v "node-ipc:/opt/cardano/ipc"
    lappend cmd nessusio/cardano-node cardano-cli
  }
  if {$show} {
    set cmdout "cardano-cli"
    foreach tok $arglst {
      if {[string length $tok] > 0} {
        set isopt [string match "--*" $tok]
        if {$isopt} { append cmdout "\n" }
        append cmdout " $tok"
      }
    }
    logInfo $cmdout
  }
  file mkdir $SCRATCH_DIR
  exec {*}[concat $cmd $arglst]
}

proc cliQueryProtocolParameters {} {
  set args [networkAwareCmd [list "query" "protocol-parameters"]]
  lappend args "--out-file" "/var/cardano/local/scratch/protocol.json"
  cardano-cli $args false
}

proc cliQueryTip {} {
  set args [networkAwareCmd [list "query" "tip"]]
  set json [cardano-cli $args false]
  dict set tip epoch [exec echo $json | jq -r .epoch]
  dict set tip slot [exec echo $json | jq -r .slot]
  return $tip
}

proc cliTxSign {addrInfo} {
  set addrSkey [dict get $addrInfo skey]
  logInfo "Sign transaction"
  lappend args "transaction" "sign"
  lappend args "--tx-body-file" "/var/cardano/local/scratch/tx.raw"
  lappend args "--signing-key-file" "/var/cardano/local/$addrSkey"
  lappend args "--out-file" "/var/cardano/local/scratch/tx.signed"
  cardano-cli $args false
}

# Sign the Tx
proc cliTxSubmit {addrInfo txid} {
  global TTL
  logInfo "Submit transaction "
  set args [networkAwareCmd [list "transaction" "submit"]]
  lappend args "--tx-file" "/var/cardano/local/scratch/tx.signed"
  cardano-cli $args false

  set count 0
  set waitms 3000
  set txids [list $txid]
  while {$count < [expr {1000 * $TTL / $waitms}] && [lsearch $txids $txid] >= 0} {
    puts -nonewline "."; flush stdout
    set utxos [queryUtxos $addrInfo false]
    set txids [dict keys $utxos]
    after $waitms
    incr count
  }
  puts ""
  set success [expr {[lsearch $txids $txid] < 0}]
  if {!$success} {
    logError "Timeout, aborting Tx monitor"
  }
  return $success
}

proc fetchBlockfrostData {path {params ""}} {
  global BLOCKFROST_API_KEY
  global BLOCKFROST_API_URL
  set bfheader "project_id:$BLOCKFROST_API_KEY"
  set bfurl "$BLOCKFROST_API_URL/$path"
  if {$params != ""} { append bfurl "?$params" }
  set json [exec curl -sH $bfheader $bfurl]
  if {[string first "status_code" $json] > 0} {
    set status [exec echo $json | jq -r .status_code]
    if {$status > 200} {
      set message [exec echo $json | jq -r .message]
      error "$status $message"
    }
  }
  return $json
}

proc filterUtxosBySymbol {utxos symbol} {
  set result [dict create]
  foreach txid [dict keys $utxos] {
    set value [dict get $utxos $txid value]
    set datum [dict get $utxos $txid datum]
    set symbols [dict keys $value]
    set isAdaOnly [expr {[llength $symbols] == 1}]
    set hasTokens [expr {[lsearch $symbols $symbol] >= 0}]
    if {$isAdaOnly && $symbol == "lovelace" || $hasTokens && $symbol != "lovelace"} {
      dict set result $txid value $value
      dict set result $txid datum $datum
    }
  }
  return $result
}

proc filterUtxosByDatum {utxos datumHash} {
  set result [dict create]
  foreach txid [dict keys $utxos] {
    set value [dict get $utxos $txid value]
    set datum [dict get $utxos $txid datum]
    if {$datum == $datumHash} {
      dict set result $txid value $value
      dict set result $txid datum $datum
    }
  }
  return $result
}

proc findCollateralId {utxos {feesId ""}} {
  global MIN_COLLATERAL
  set collateralId ""
  foreach txid [dict keys $utxos] {
    set isFeesId [expr {$feesId == $txid}]
    set subdct [dict get $utxos $txid value]
    set symbols [dict keys $subdct]
    set amount [dict get $subdct "lovelace"]
    set isAdaOnly [expr {[llength $symbols] == 1}]
    if {$isAdaOnly && !$isFeesId && $MIN_COLLATERAL <= $amount} {
      set collateralId $txid
      break
    }
  }
  if {$collateralId == ""} {
      error "Cannot find UTxO for collateral"
  }
  return $collateralId
}

proc findFeesId {utxos {minAmount 1000000}} {
  set feesId ""
  foreach txid [dict keys $utxos] {
    set subdct [dict get $utxos $txid value]
    set symbols [dict keys $subdct]
    set amount [dict get $subdct "lovelace"]
    set isAdaOnly [expr {[llength $symbols] == 1}]
    if {$isAdaOnly && $minAmount <=  $amount} {
      set feesId $txid
      break
    }
  }
  if {$feesId == ""} {
      error "Cannot find UTxO for fees"
  }
  return $feesId
}

proc getAddrInfo {key} {
  global addrMap
  dict get $addrMap $key
}

proc getAddrInfoByName {name} {
  switch -nocase $name {
    Owner   { getAddrInfo 0 }
    Shelley { getAddrInfo 1 }
    Mary    { getAddrInfo 2 }
    Percy   { getAddrInfo 3 }
    Script  { getAddrInfo 4 }
    default { error "Unknown wallet name: $name" }
  }
}

proc getCurrentEpoch {} {
  set json [fetchBlockfrostData "epochs/latest"]
  exec echo $json | jq -r .epoch
}

proc getCurrencySymbol {assetClass} {
  set i [string first "." $assetClass]
  if {$i < 1} { error "Invalid asset class: $assetClass"}
  set symbol [string range $assetClass 0 [expr {$i - 1}]]
}

proc getDatumHash {tokenName} {
  global SCRATCH_DIR
  global TOKEN_TTL_EPOCHS
  set epoch [getEpochFromTokenName $tokenName]
  set fpath "$SCRATCH_DIR/script-datum$epoch.json"
  if {![file exists $fpath]} {
    set ttl "e[expr {$epoch + $TOKEN_TTL_EPOCHS}]"
    set invalidAfter [getInvalidAfterTime $ttl]
    return [writeDatumFile $tokenName $invalidAfter]
  }

  set datjson [fileRead $fpath]
  set invalidAfterMillis [exec echo $datjson | jq -r {.fields[1].int}]
  set invalidAfterTime [expr {$invalidAfterMillis / 1000}]
  set timestr [clock format $invalidAfterTime -format {%d-%m-%Y %H:%M:%S %Z}]
  logInfo "ScriptDatum: $tokenName => $datjson"
  logInfo "Invalid after time: $invalidAfterTime => $timestr"

  lappend args "transaction" "hash-script-data"
  lappend args "--script-data-file" "/var/cardano/local/scratch/script-datum$epoch.json"
  set datumHash [cardano-cli $args false]
  return $datumHash
}

proc getEpochBoundaries {{epoch "latest"}} {
  set byronSecondsPerSlot 20
  set byronSlotsPerEpoch 21600
  set shelleySlotsPerEpoch 432000
  if [isNetwork "testnet"] {
    set shelleyStartEpoch 74
    set networkStartTime 1563999616
  } else { if [isNetwork "mainnet"] {
    set shelleyStartEpoch 208
    set networkStartTime 1506203091
  }}

  if {$epoch == "latest"} { set epoch [getCurrentEpoch] }
  if {$epoch < $shelleyStartEpoch} { error "Pre Shelley epoch not supported: $epoch" }

  set shelleyStartSlot [expr {$shelleyStartEpoch * $byronSlotsPerEpoch}]
  set shelleyStartTime [expr {$networkStartTime + $shelleyStartSlot * $byronSecondsPerSlot}]
  set epochDelta [expr {$epoch - $shelleyStartEpoch}]

  set currentTime [clock seconds]
  dict set bounds currentTime $currentTime
  dict set bounds currentSlot [expr {$shelleyStartSlot + $currentTime - $shelleyStartTime}]

  dict set bounds epoch $epoch
  dict set bounds epochStartSlot [expr {$shelleyStartSlot + $epochDelta * $shelleySlotsPerEpoch}]
  dict set bounds epochStartTime [expr {$shelleyStartTime + $epochDelta * $shelleySlotsPerEpoch}]
  return $bounds
}

proc getEpochFromTokenName {tokenName} {
  if {[string first "Astor" $tokenName] != 0} {
    error "Invalid token name: $tokenName"
  }
  set epoch [string range $tokenName 5 end]
}

proc getEpochStartTime {epoch} {
  set json [fetchBlockfrostData "epochs/$epoch"]
  exec echo $json | jq .start_time
}

proc getInvalidAfterTime {ttl} {
  global TOKEN_TTL_EPOCHS
  set result 0

  # Use the default TTL
  if {$ttl == 0} {
    set curEpoch [getCurrentEpoch]
    set ttlEpoch [expr {$curEpoch + $TOKEN_TTL_EPOCHS}]
    set ttl "e$ttlEpoch"
  }

  # Is a relative time spec in seconds
  if {[string index $ttl 0] == "+"} {
    set delta [string range $ttl 1 end]
    set result [expr {[clock seconds] + $delta}]
  }

  # Is the start time of an epoch
  if {[string index $ttl 0] == "e"} {
    set epoch [string range $ttl 1 end]
    set bounds [getEpochBoundaries $epoch]
    set result [dict get $bounds epochStartTime]
  }

  # Is an absolute time spec in seconds
  if {$result == 0 && [string is integer $ttl]} {
    set result $ttl
  }

  if {$result == 0} {
    error "Invalid ttl spec: $ttl"
  }

  set timestr [clock format $result -format {%d-%m-%Y %H:%M:%S %Z}]
  logInfo "Invalid after time: $ttl => $result => $timestr"

  return $result
}

proc getProtocolConfig {} {
  global SCRATCH_DIR
  set protocol "/var/cardano/local/scratch/protocol.json"
  if {![file exists $protocol]} {
    set args [networkAwareCmd [list "query" "protocol-parameters"]]
    lappend args "--out-file" $protocol
    puts [cardano-cli $args false]
  }
  return $protocol
}

proc getSectionHeader {title} {
  set padding ""
  while {[string length "$title $padding"] < 180} {
    set padding "$padding="
  }
  return "$title $padding"
}

proc isSwapV1 {epoch} {
  set mainnet [isNetwork "mainnet"]
  set boundary [expr {$mainnet ? 314 : 180}]
  expr $epoch < $boundary
}

proc getSwapAmount {epoch amount} {
  set multiplier [expr {[isSwapV1 $epoch] ? 1000 : 10}]
  expr {$amount * $multiplier * 1000}
}

proc getSwapScriptFile {epoch} {
  global SCRIPTS_DIR
  set suffix [expr {[isSwapV1 $epoch] ? "v1" : "v2"}]
  return "/var/cardano/local/$SCRIPTS_DIR/astorswap$suffix.plutus"
}

proc getSwapScriptInfo {epoch} {
  global SCRIPT_ADDR_V1
  global SCRIPT_ADDR_V2
  if {[isSwapV1 $epoch]} {
    dict set scriptInfo name "ScriptV1"
    dict set scriptInfo addr $SCRIPT_ADDR_V1
  } else {
    dict set scriptInfo name "ScriptV2"
    dict set scriptInfo addr $SCRIPT_ADDR_V2
  }
  return $scriptInfo
}

proc getTokenName {assetClass} {
    set toks [split $assetClass "."]
    if {[llength $toks] != 2} { error "Invalid asset class: $assetClass"}
    lindex $toks [expr {[llength $toks] - 1}]
}

proc getTxinSpecs {fromInfo utxos txoutSpecs} {
  global MIN_PLUTUS_FEES
  global MIN_TOKEN_LOVELACE
  set fromName [dict get $fromInfo name]

  logDebug "Get TxinSpecs from TxoutSpecs $fromName $txoutSpecs"

  # Find the UTXOs that sattisfy the txoutSpecs
  dict set txinSpecs "lovelace" txids [list]
  dict set txinSpecs "lovelace" amount 0

  set txoutAmounts [getTxoutAmounts $fromInfo $txoutSpecs]
  set symbols [dict keys $txoutAmounts]
  set numTokenTxouts 0
  foreach idx [dict keys $txoutSpecs] {
    set value [dict get $txoutSpecs $idx txoutValue]
    if {[lindex [split $value] 0] != "lovelace"} {
      incr numTokenTxouts
    }
  }
  set hasTokens [expr {$numTokenTxouts > 0}]
  logDebug "Num Token Txouts: $numTokenTxouts"

  if {$hasTokens} {
    if {[llength $symbols] != 2} { error "Unsupported number of symbols: $txoutAmounts" }
    if {[lindex $symbols 0] != "lovelace"} { error "Unexpected symbol at index 0: $txoutAmounts" }
    set tokenUtxos [filterUtxosBySymbol $utxos [lindex $symbols 1]]
  }

  foreach symbol $symbols {
    if {![dict exists $txinSpecs $symbol]} {
      dict set txinSpecs $symbol txids [list]
      dict set txinSpecs $symbol amount 0
    }

    set targetAmount [dict get $txoutAmounts $symbol]
    logDebug "Target amount: $targetAmount $symbol"
    if {$symbol == "lovelace" && $hasTokens} {
      set tkoutLovelace [expr {$MIN_TOKEN_LOVELACE + $MIN_PLUTUS_FEES}]
      incr targetAmount [expr {($numTokenTxouts) * $tkoutLovelace}]
      logDebug "Adjusted target amount: $targetAmount $symbol"
    }

    set symbolUtxos [filterUtxosBySymbol $utxos $symbol]
    foreach txid [dict keys $symbolUtxos] {
      logDebug "Processing: $txid"
      set subdct [dict get $symbolUtxos $txid value]
      set txids [dict get $txinSpecs $symbol txids]
      set auxamt [dict get $txinSpecs $symbol amount]
      set amount [dict get $subdct $symbol]
      if {[lsearch $txids $txid] < 0} {
        if {$symbol == "lovelace"} {
          if {$auxamt <= [expr {$targetAmount + $MIN_TOKEN_LOVELACE}]} {
            dict set txinSpecs $symbol txids [lappend txids $txid]
            dict set txinSpecs $symbol amount [expr {$auxamt + $amount}]
            logDebug "Added to $symbol: [dict get $txinSpecs $symbol]"
          }
        } else {
          if {$auxamt < $targetAmount} {
            dict set txinSpecs $symbol txids [lappend txids $txid]
            dict set txinSpecs $symbol amount [expr {$auxamt + $amount}]
            logDebug "Added to $symbol: [dict get $txinSpecs $symbol]"
            set auxamt [dict get $txinSpecs "lovelace" amount]
            set amount [dict get $subdct "lovelace"]
            dict set txinSpecs "lovelace" amount [expr {$auxamt + $amount}]
            logDebug "Added to lovelace: [dict get $txinSpecs lovelace]"
          }
        }
      }
    }
  }

  logDebug "TxinSpecs: $txinSpecs"
  return $txinSpecs
}

proc getTxoutAmounts {fromInfo txoutSpecs {verbose false}} {
  set fromName [dict get $fromInfo name]

  # Collect the target amounts
  dict set txoutAmounts "lovelace" 0
  foreach i [dict keys $txoutSpecs] {
    set subdct [dict get $txoutSpecs $i]
    set txoutAddr [dict get $subdct txoutAddress]
    set txoutValue [dict get $subdct txoutValue]
    set toutAddrName [dict get $subdct txoutAddress name]
    foreach symbol [dict keys $txoutValue] {
      if {$symbol != "lovelace"} { getTokenName $symbol }
      set amount [dict get $txoutValue $symbol]
      if {![dict exists $txoutAmounts $symbol]} {
        dict set txoutAmounts $symbol 0
      }
      set auxamt [dict get $txoutAmounts $symbol]
      dict set txoutAmounts $symbol [expr {$auxamt + $amount}]
      if { $verbose } {
        logInfo "Pay $fromName => $toutAddrName $amount $symbol"
      }
    }
  }
  if { $verbose } {
    foreach symbol [dict keys $txoutAmounts] {
      set amount [dict get $txoutAmounts $symbol]
      logInfo "In total $amount $symbol"
    }
  }
  return $txoutAmounts
}

# Delete an elemet from a list
proc ldelete {lst idx} {
    return [lreplace $lst $idx $idx]
}

proc networkAwareCmd {opts} {
  global BLOCKFROST_NETWORK
  global TESTNET_MAGIC
  if {$BLOCKFROST_NETWORK == "testnet"} {
    lappend opts --testnet-magic $TESTNET_MAGIC
  } else { if {$BLOCKFROST_NETWORK == "mainnet"} {
    lappend opts --mainnet
  }}
}

# Tcl 8.6: binary decode hex 48656c6c6f2c20776f726c6421
proc stringFromHEX {hex} {
  binary format  H*  $hex
}

# Tcl 8.6: binary encode hex "Hello, world!"
proc stringToHEX {str} {
  set  ab  [binary format a* $str]
  binary scan  $ab  H* str
  return $str
}

proc toAssetClass {policyId tokenName} {
  return "$policyId.$tokenName"
}

proc toAdaValue {ada} {
  toValue "lovelace" [toLovelace $ada]
}

proc toLovelace {ada} {
  expr {$ada * 1000000}
}

proc toTxoutSpecs {toInfo symbol amount} {
  dict set txoutSpecs 0 txoutAddress name [dict get $toInfo name]
  dict set txoutSpecs 0 txoutAddress addr [dict get $toInfo addr]
  dict set txoutSpecs 0 txoutValue [toValue $symbol $amount]
  return $txoutSpecs
}

proc toValue {symbol amount} {
  dict set value $symbol $amount
}

proc writeDatumFile {tokenName invalidAfterTime} {
  global SCRATCH_DIR
  set epoch [getEpochFromTokenName $tokenName]
  set encoded [exec echo -n $tokenName | xxd -ps]
  set invalidAfterMillis [expr {$invalidAfterTime * 1000}]
  set datjson {{"constructor":0,"fields":[{"bytes":"#field1#"},{"int":#field2#}]}}
  set datjson [string map [list "#field1#" $encoded] $datjson]
  set datjson [string map [list "#field2#" $invalidAfterMillis] $datjson]
  fileWrite "$SCRATCH_DIR/script-datum$epoch.json" $datjson
  return [getDatumHash $tokenName]
}
