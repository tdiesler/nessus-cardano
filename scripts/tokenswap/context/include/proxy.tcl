
proc astorProxy {opts} {
  dict set spec "--proxy" [dict create required true]
  set args [argsInit $spec $opts]
  set cmd [argsValue $args "--proxy"]
  switch $cmd {
    run     { astorProxyRun $opts }
    test    { astorProxyTest $opts }
    default { logError "Invalid command: $cmd"; astorHelp }
  }
}

proc astorProxyRun {opts} {
  global percyInfo
  global scriptInfo

  dict set spec "--endless" [dict create required false]
  set args [argsInit $spec $opts]
  set intrv [argsValue $args "--intrv" 600]
  set endless [argsValue $args "--endless" true]

  set PROXY_FROM_NAME [envvar "PROXY_FROM_NAME" [dict get $percyInfo name]]
  set PROXY_FROM_ADDR [envvar "PROXY_FROM_ADDR" [dict get $percyInfo addr]]
  set PROXY_FROM_SKEY [envvar "PROXY_FROM_SKEY" [dict get $percyInfo skey]]

  dict set fromInfo name $PROXY_FROM_NAME
  dict set fromInfo addr $PROXY_FROM_ADDR
  dict set fromInfo skey $PROXY_FROM_SKEY

  runProxyLoop $fromInfo $scriptInfo $intrv $endless
}

proc astorProxyTest {opts} {
  logInfo [getSectionHeader "astor $opts"]

  dict set spec "--epoch" [dict create required false]
  set args [argsInit $spec $opts]
  set epoch [argsValue $args "--epoch" [getCurrentEpoch]]

  set swaps 4
  set values "10 Ada 10 Ada"
  for {set i 0} {$i < $swaps} {incr i} {
    set values [concat $values "1000 Astor$epoch"]
  }

  astor [list --reset --epoch $epoch]
  astor [list --pay2pkh --from Owner --to Shelley --value $values]
  astor [list --pay2pkh --from Owner --to Percy --value "10 Ada 10 Ada"]
  astor [list --pay2script --from Owner --epoch $epoch --value "100 Ada"]

  for {set i 0} {$i < $swaps} {incr i} {
    astor [list --pay2pkh --from Shelley --to Percy --value "1000 Astor$epoch"]
    after 30
  }
  astor [list --show all]
}

proc getCallerAddress {txhash tokenName} {
  global POLICY_ID
  set json [fetchBlockfrostData "txs/$txhash/utxos"]
  set unit "$POLICY_ID[exec echo -n $tokenName | xxd -ps]"
  set jquery ".inputs\[] | select(.amount\[].unit == \"$unit\") | .address"
  set jqres [splitTrim [exec echo $json | jq -r $jquery] '\n']
  if {[llength $jqres] < 1} { error "Unexpected address result: $jqres"}
  set result [lindex $jqres 0]
  logInfo "CallerAddr: $txhash => $result"
  return $result
}

proc handleToScript {fromInfo scriptInfo utxos txid assetClass} {
  set fromName [dict get $fromInfo name]
  set scriptAddr [dict get $scriptInfo addr]
  set tokenName [getTokenName $assetClass]
  set epoch [getEpochFromTokenName $tokenName]

  set txhash [lindex [split $txid '#'] 0]
  set targetAddr [getCallerAddress $txhash $tokenName]

  set value [dict get $utxos $txid value]
  set amount [dict get $value $assetClass]

  logInfo [getSectionHeader "Proxy Swap $amount $tokenName from $fromName to $targetAddr"]
  scriptSwapTokens $fromInfo $amount $tokenName $targetAddr
}

proc handleUtxos {fromInfo scriptInfo utxos} {
  foreach txid [dict keys $utxos] {
    set value [dict get $utxos $txid value]
    set assetClasses [dict keys $value]
    foreach assetClass [ldelete $assetClasses 0] {
      set tokenName [getTokenName $assetClass]
      if {[string match "Astor*" $tokenName]} {
        set epoch [getEpochFromTokenName $tokenName]
        set scriptInfo [getSwapScriptInfo $epoch]
        handleToScript $fromInfo $scriptInfo $utxos $txid $assetClass
        queryUtxos $fromInfo
        queryUtxos $scriptInfo
      }
    }
  }
}

proc runProxyLoop {fromInfo scriptInfo intrv endless} {
  set fromName [dict get $fromInfo name]
  set fromAddr [dict get $fromInfo addr]
  set scriptAddr [dict get $scriptInfo addr]
  logInfo ""
  logInfo "Run proxy loop"
  logInfo " --from $fromName $fromAddr"
  logInfo " --script $scriptAddr"
  set i 0
  set wait 5
  if {[expr {$intrv % $wait != 0}]} { error "Interval not divisible by: $wait"}
  while {$i == 0 || $endless} {
    set logQuery [expr {$i % $intrv == 0}]
    set utxos [queryUtxos $fromInfo $logQuery]
    handleUtxos $fromInfo $scriptInfo $utxos
    if {$logQuery} { queryUtxos $scriptInfo $logQuery }
    after [expr {$wait * 1000}]
    incr i $wait
  }
}
