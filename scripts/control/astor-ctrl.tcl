#! /usr/local/bin/tclsh

set scripPath [file normalize [info script]]
set tcldir [file dirname $scripPath]

proc findPath {srcdir fpath} {
  set result "$srcdir/$fpath"
  set parent [file normalize "$srcdir/.."]
  if {[file exists $result]} { return $result }
  if {$parent == "/"} { error "Cannot find $fpath" }
  return [findPath $parent $fpath]
}

source [findPath $tcldir "common/essentials.tcl"]

set LOG_LEVEL $LEVEL_INFO

# Astor Main Procs #############################################################
#

proc controlHelp {} {
  puts ""
  puts "Usage"
  puts "------------------------------------------------------------------------"
  puts "astor-ctrl --generate-schedule --leaderlog ~/leaderlog.json --out-file ~/cardano/restart-schedule.csv"
  puts "astor-ctrl --run-schedule --type leaderlog --cname bprod --schedule ~/cardano/restart-schedule.csv"
  puts "astor-ctrl --run-schedule --schedule ~/cardano/restart-schedule.csv"
  puts ""
  puts "# Generic Action Schedule"
  puts "# timestamp, cmd"
}

proc generateLeaderlogSchedule {opts} {
  dict set spec "--leaderlog" [dict create required true]
  dict set spec "--out-file" [dict create required true]
  set args [argsInit $spec $opts]
  set inpath [argsValue $args "--leaderlog"]
  set outpath [argsValue $args "--out-file"]

  logInfo [getSectionHeader "Parse $inpath"]

  # Parse already existing restart schedule
  set schedule [parseLeaderlogSchedule $outpath]

  # Get timestamp of the last block
  set tsLastBlock 0
  foreach no [dict keys $schedule] {
    set tsLastBlock [dict get $schedule $no tsblock]
  }

  # Parse cncli generated leaderlog
  set content [fileRead $inpath]
  set epoch [exec echo $content | jq -r ".epoch"]
  set parsed [exec echo $content | jq -c ".assignedSlots\[] | \{no: .no, slot: .slotInEpoch, at: .at\}"]
  foreach line [split $parsed '\n'] {
    set no [expr {100 * $epoch + [exec echo $line | jq -r .no]}]
    set slot [exec echo $line | jq -r .slot]
    set at [exec echo $line | jq -r .at]
    set tsblock [clock scan $at -format "%Y-%m-%dT%T%z"]
    set tsrestart [expr {$tsblock - 7200}]
    set tsrestart [expr {$tsLastBlock + 180 < $tsrestart ? $tsrestart : 0}]
    dict set schedule $no slot $slot
    dict set schedule $no at $at
    dict set schedule $no tsblock $tsblock
    dict set schedule $no tsrestart $tsrestart
    set tstrB [clock format $tsblock -format "%Y-%m-%d %T"]
    set tstrR [clock format $tsrestart -format "%Y-%m-%d %T"]
    logInfo "$no $slot [expr {$tsrestart == 0 ? "n/a" : $tstrR}] => $tstrB"
    set tsLastBlock $tsblock
  }

  logInfo [getSectionHeader "Generate $outpath"]

  # Write output file
  set outfile [open $outpath "w"]
  puts $outfile "# no,slot,block,restart"
  foreach no [dict keys $schedule] {
    set prevEpoch [expr {$epoch - 1}]
    set isPrevEpoch [string match "${prevEpoch}*" $no]
    set isCurrEpoch [string match "${epoch}*" $no]
    if {$isPrevEpoch || $isCurrEpoch} {
      set slot [dict get $schedule $no slot]
      set tsblock [dict get $schedule $no tsblock]
      set tsrestart [dict get $schedule $no tsrestart]
      puts $outfile "$no,$slot,$tsblock,$tsrestart"
      logInfo "$no,$slot,$tsblock,$tsrestart"
    }
  }
  close $outfile
}

proc getTimeDiffString {tdiff} {
  set result [clock format $tdiff -gmt true -format "%T"]
  if {$tdiff >= 24 * 3600} {
    set days [expr {$tdiff / (24 * 3600)}]
    set tdiff [expr {$tdiff - $days * 24 * 3600}]
    set result [clock format $tdiff -gmt true -format "${days}d %T"]
  }
  return $result
}

proc parseGenericSchedule {inpath} {
  set schedule [dict create]
  if {[file isfile $inpath]} {
    set content [fileRead $inpath]
    foreach line [splitTrim $content '\n'] {
      set toks [splitTrim $line ',']
      if {[string match "#*" $line]} { continue }
      if {[llength $toks] != 2} { error "Invalid schedule: $line"}
      set at [lindex $toks 0]
      set action [lindex $toks 1]
      if {[string match "+*" $at]} {
        set now [clock seconds]
        set tstamp [expr {$now + [string range $at 1 end]}]
      } else { if {[string match "20??-*" $at]} {
        set tstamp [clock scan $at -format "%Y-%m-%d %T"]
      }}
      dict set schedule $tstamp $action
      logInfo "$tstamp $action"
    }
  }
  return $schedule
}

proc parseLeaderlogSchedule {inpath} {
  set schedule [dict create]
  if {[file isfile $inpath]} {
    set content [fileRead $inpath]
    foreach line [splitTrim $content '\n'] {
      set toks [splitTrim $line ',']
      if {[string match "#*" $line]} { continue }
      if {[llength $toks] != 4} { error "Invalid schedule: $line"}
      set no [lindex $toks 0]
      set slot [lindex $toks 1]
      set tsblock [lindex $toks 2]
      set tsrestart [lindex $toks 3]
      dict set schedule $no slot $slot
      dict set schedule $no tsblock $tsblock
      dict set schedule $no tsrestart $tsrestart
      set tstrB [clock format $tsblock -format "%Y-%m-%d %T"]
      set tstrR [clock format $tsrestart -format "%Y-%m-%d %T"]
      logInfo "$no $slot [expr {$tsrestart == 0 ? "n/a" : $tstrR}] => $tstrB"
    }
  }
  return $schedule
}

proc runGenericSchedule {opts} {
  dict set spec "--schedule" [dict create required true]
  dict set spec "--logsec" [dict create required false]
  set args [argsInit $spec $opts]
  set cname [argsValue $args "--cname"]
  set inpath [argsValue $args "--schedule"]
  set logsec [argsValue $args "--logsec" 600]

  logInfo [getSectionHeader "Run Generic Schedule $inpath"]

  # Read the generated tsrestart schedule
  set schedule [parseGenericSchedule $inpath]
  if {[dict size $schedule] == 0} { error "Empty schedule: $inpath"}

  set i 0
  set wait 5
  if {[expr {$logsec % $wait != 0}]} { error "Interval not divisible by: $wait"}
  foreach tstamp [dict keys $schedule] {
    set action [dict get $schedule $tstamp]
    set now [clock seconds]
    if {$now < $tstamp} {
      set tstr [clock format $tstamp -format "%Y-%m-%d %T"]
      logInfo [getSectionHeader "Next $tstr => $action"]
      while {$now < $tstamp} {
        if {[expr {$i % $logsec == 0}]} {
          set tdiff [expr {$tstamp - $now}]
          logInfo "Run '$action' in [getTimeDiffString $tdiff]"
        }
        incr i $wait
        after [expr {$wait * 1000}]
        set now [clock seconds]
      }
      logInfo "Run '$action'"
      logInfo [exec {*}$action]
    }
  }
}

proc runLeaderlogSchedule {opts} {
  dict set spec "--cname" [dict create required true]
  dict set spec "--schedule" [dict create required true]
  dict set spec "--logsec" [dict create required false]
  set args [argsInit $spec $opts]
  set cname [argsValue $args "--cname"]
  set inpath [argsValue $args "--schedule"]
  set logsec [argsValue $args "--logsec" 600]

  logInfo [getSectionHeader "Run Leaderlog Schedule $inpath"]

  # Read the generated tsrestart schedule
  set schedule [parseLeaderlogSchedule $inpath]

  set i 0
  set wait 5
  if {[expr {$logsec % $wait != 0}]} { error "Interval not divisible by: $wait"}
  foreach no [dict keys $schedule] {
    set now [clock seconds]
    set slot [dict get $schedule $no slot]
    set tsblock [dict get $schedule $no tsblock]
    set tsrestart [dict get $schedule $no tsrestart]
    if {$now < $tsrestart} {
      set tstrB [clock format $tsblock -format "%Y-%m-%d %T"]
      set tstrR [clock format $tsrestart -format "%Y-%m-%d %T"]
      logInfo [getSectionHeader "Next $no $slot $tstrR => $tstrB"]
      while {$now < $tsrestart} {
        if {[expr {$i % $logsec == 0}]} {
          set tdiff [expr {$tsrestart - $now}]
          logInfo "Restarting $cname in [getTimeDiffString $tdiff]"
        }
        incr i $wait
        after [expr {$wait * 1000}]
        set now [clock seconds]
      }
      logInfo "Restarting $no $slot $tstrR => $tstrB"
      logInfo "docker restart $cname"
      logInfo [exec docker restart $cname]
    }
  }
}

proc runSchedule {opts} {
  dict set spec "--type" [dict create required false]
  set args [argsInit $spec $opts]
  set type [argsValue $args "--type" "generic"]
  switch $type {
    leaderlog { runLeaderlogSchedule $opts }
    default   { runGenericSchedule $opts }
  }
}

# Astor Control Main Entry #####################################################
#

set secBefore [clock seconds]

if {[llength $argv] == 0} {
  controlHelp
  return
}

switch [lindex $argv 0] {
  --generate-schedule { generateLeaderlogSchedule $argv }
  --run-schedule      { runSchedule $argv }
  default             { controlHelp }
}

puts ""
set secDiff [expr {[clock seconds] - $secBefore}]
puts "Done in [clock format $secDiff -format {%M:%S}]"
