#! /usr/local/bin/tclsh

set scripPath [file normalize [info script]]
set tcldir [file dirname $scripPath]

source $tcldir/include/constants.tcl
source $tcldir/include/utils.tcl

set MONITOR_TICKER_LIST         [envvar "MONITOR_TICKER_LIST" "UNDEFINED"]
set MONITOR_TICKER_SIZE         [envvar "MONITOR_TICKER_SIZE" 100]
set MONITOR_MAX_FIXED           [envvar "MONITOR_MAX_FIXED" 350000000]
set MONITOR_MAX_FIXED_INCREASE  [envvar "MONITOR_MAX_FIXED_INCREASE" 0.0]
set MONITOR_MAX_MARGIN          [envvar "MONITOR_MAX_MARGIN" 0.03]
set MONITOR_MAX_MARGIN_INCREASE [envvar "MONITOR_MAX_MARGIN_INCREASE" 0.0]
set MONITOR_ENDLESS             [envvar "MONITOR_ENDLESS" true]
set MONITOR_SLEEP               [envvar "MONITOR_SLEEP" 2000]

# Main Entrypoint ##############################################################
#

logInfo "BLOCKFROST_NETWORK=$BLOCKFROST_NETWORK"
logInfo "MONITOR_TICKER_LIST=$MONITOR_TICKER_LIST"
logInfo "MONITOR_TICKER_SIZE=$MONITOR_TICKER_SIZE"
logInfo "MONITOR_MAX_FIXED=$MONITOR_MAX_FIXED"
logInfo "MONITOR_MAX_FIXED_INCREASE=$MONITOR_MAX_FIXED_INCREASE"
logInfo "MONITOR_MAX_MARGIN=$MONITOR_MAX_MARGIN"
logInfo "MONITOR_MAX_MARGIN_INCREASE=$MONITOR_MAX_MARGIN_INCREASE"
logInfo "MONITOR_ENDLESS=$MONITOR_ENDLESS"
logInfo "MONITOR_SLEEP=$MONITOR_SLEEP"

set poolData [initStakePoolData $MONITOR_TICKER_SIZE]
set tickers [lsort [getPoolTickers $poolData]]
set datsize [llength $tickers]

logInfo "Monitor $datsize stake pools"

set i 0
while { $i < $datsize || $MONITOR_ENDLESS } {
  set ticker [lindex $tickers [expr {$i % $datsize}]]
  set poolId [getPoolIdForTicker $poolData $ticker]
  set initFixed [dict get $poolData $poolId fixed]
  set initMargin [dict get $poolData $poolId margin]
  set nextCost [getStakePoolCost $poolId]
  set nextFixed [dict get $nextCost fixed]
  set nextMargin [dict get $nextCost margin]

  # Fake cost increase for testing
  #if {$i > $datsize} { set nextMargin [expr $nextMargin + 0.5] }
  #if {$i > $datsize} { set nextFixed [expr $nextFixed + 100 * 1000000] }

  set fixedIncr [expr {(1.0 * $nextFixed)/max($initFixed,0.001) - 1}]
  set fixedBad [expr {$MONITOR_MAX_FIXED < $nextFixed|| $MONITOR_MAX_FIXED_INCREASE < $fixedIncr}]

  set marginIncr [expr {$nextMargin/max($initMargin,0.001) - 1}]
  set marginBad [expr {$MONITOR_MAX_MARGIN < $nextMargin || $MONITOR_MAX_MARGIN_INCREASE < $marginIncr}]

  set message "$poolId $ticker \[$nextFixed/$initFixed, $nextMargin/$initMargin]"
  if { $fixedBad } {
    logWarn "$message - Bad Fixed Cost"
  } else { if { $marginBad }  {
    logWarn "$message - Bad Margin Cost"
  } else {
    logInfo "$message - OK"
  }}

  after $MONITOR_SLEEP
  incr i
}
