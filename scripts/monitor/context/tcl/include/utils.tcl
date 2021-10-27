
# Utility procs ################################################################
#

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

proc fileRead {fpath} {
  set infile [open $fpath "r"]
  set result [gets $infile]
  close $infile
  return $result
}

proc fileWrite {fpath content} {
  set outfile [open $fpath "w"]
  puts $outfile $content
  close $outfile
}

proc getStakePoolCost {poolId} {
  set json [fetchBlockfrostData "/pools/$poolId"]
  set margin [exec echo $json | jq -r .margin_cost]
  set fixed [exec echo $json | jq -r .fixed_cost]
  dict set poolcost margin $margin
  dict set poolcost fixed $fixed
  return $poolcost
}

proc getPoolIdForTicker {pooldat ticker} {
  foreach poolId [dict keys $pooldat] {
    if {$ticker == [dict get $pooldat $poolId ticker]} {
      return $poolId
    }
  }
  error "Unknown ticker: $ticker"
}

proc getPoolTickers {pooldat} {
  foreach poolId [dict keys $pooldat] {
    lappend result [dict get $pooldat $poolId ticker]
  }
  return $result
}

# Get the list of all stake pool ids
#
proc initStakePoolData {maxsize} {
  global MONITOR_TICKER_LIST
  set count 100
  set datsize 0
  set tick 0
  set tickerList [splitTrim $MONITOR_TICKER_LIST]
  set isUndefined [expr {$MONITOR_TICKER_LIST == "UNDEFINED"}]
  set expDataSize [expr {$isUndefined ? $maxsize : [llength $tickerList]}]
  logInfo "Start monitoring $expDataSize pools"
  for {set page 1} {$datsize < $expDataSize} { incr page } {
    set json [fetchBlockfrostData "/pools" "page=$page&count=$count"]
    set poolIds [exec echo $json | jq -r {.[]}]
    set poolCount [llength $poolIds]
    if {$poolCount == 0} {
      logWarn "No more pool ids"
      break
    }
    set upper [expr {$page * $count}]
    set lower [expr {$upper - $count + 1}]
    logInfo "Loading metadata for \[$lower-$upper] stake pools"
    foreach poolId $poolIds {
      set json [fetchBlockfrostData "/pools/$poolId/metadata"]
      set ticker [exec echo $json | jq -r {.ticker}]
      if {$ticker != "null" && ($isUndefined || [lsearch $tickerList $ticker] >= 0)} {
        set poolcost [getStakePoolCost $poolId]
        dict set pooldat $poolId ticker $ticker
        dict set pooldat $poolId fixed [dict get $poolcost fixed]
        dict set pooldat $poolId margin [dict get $poolcost margin]
        set datsize [dict size $pooldat]
      }
      if {$expDataSize <= $datsize} { break }
      if {[expr {[incr tick] % 20 == 0}]} {
        puts -nonewline $datsize
        flush stdout
      } else {
        puts -nonewline "."
        flush stdout
      }
    }
    puts ""
  }
  return $pooldat
}

# Delete an elemet from a list
proc ldelete {lst idx} {
    return [lreplace $lst $idx $idx]
}

proc logDebug {message} {
  global LEVEL_DEBUG
  logMessage $LEVEL_DEBUG $message
}

proc logInfo {message} {
  global LEVEL_INFO
  logMessage $LEVEL_INFO $message
}

proc logWarn {message} {
  global LEVEL_WARN
  logMessage $LEVEL_WARN $message
}

proc logError {message} {
  global LEVEL_ERROR
  logMessage $LEVEL_ERROR $message
}

proc logMessage {level message} {
  global LOG_LEVEL
  switch $level {
      1   {set lname "DEBUG"}
      2   {set lname "INFO"}
      3   {set lname "WARN"}
      4   {set lname "ERROR"}
  }
  if {$LOG_LEVEL <= $level} {
    set now [clock seconds]
    set tstamp [clock format $now -format {%d-%m-%Y %H:%M:%S}]
    puts "\[$tstamp\] $lname $message"
  }
}

# Split and trim
proc splitTrim {instr {ch " "}} {
  set result [list]
  foreach tok [split $instr $ch] {
    set tok [string trim $tok]
    if {$tok != ""} {
      lappend result $tok
    }
  }
  return $result
}
