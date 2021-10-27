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
source $tcldir/include/constants.tcl
source $tcldir/include/contract.tcl
source $tcldir/include/proxy.tcl
source $tcldir/include/tests.tcl
source $tcldir/include/utils.tcl
source $tcldir/include/wallet.tcl

# Astor Main Procs #############################################################
#

set swaps 4

proc astorHelp {} {
  puts ""
  puts "Usage"
  puts "------------------------------------------------------------------------"
  puts "astor --reset --epoch 164"
  puts "astor --pay2pkh --from Shelley --to Owner --value all"
  puts "astor --pay2pkh --from Shelley --to Percy --value '10 Astor164'"
  puts "astor --pay2pkh --from Owner --to Shelley --value '10 Ada 10 Ada 90 Astor164 10 Astor164'"
  puts "astor --pay2pkh --from Owner --to Percy --value '10 Ada 10 Ada'"
  puts "astor --pay2pkh --from Owner --to Owner --value '30000 Ada'"
  puts "astor --pay2script --from Owner --value '100 Ada' --epoch 164"
  puts "astor --script mint --from Owner --value '1000 Astor164'"
  puts "astor --script burn --from Owner --value '1000 Astor164'"
  puts "astor --script swap --from Shelley --value '10 Astor164'"
  puts "astor --script swap --from Percy --to Shelley --value '10 Astor164'"
  puts "astor --script withdraw --from Owner --epoch 164"
  puts "astor --show all"
}

proc astorPrepare {opts} {
  global swaps
  logInfo [getSectionHeader "astor $opts"]
  dict set spec "--epoch" [dict create required false]
  set args [argsInit $spec $opts]
  set epoch [argsValue $args "--epoch" [getCurrentEpoch]]

  set values "2 Ada 2 Ada"
  for {set i 0} {$i < $swaps} {incr i} {
    set values [concat $values "10 Astor$epoch"]
  }
  set scriptAmount [expr {$swaps * 10}]

  astor [list --reset --epoch $epoch]
  astor [list --pay2pkh --from Owner --to Shelley --value $values]
  astor [list --pay2pkh --from Owner --to Percy --value "2 Ada 2 Ada"]
  astor [list --pay2script --from Owner --epoch $epoch --value "$scriptAmount Ada"]
  astor [list --show all]
}

proc astorReset {opts} {
  dict set spec "--epoch" [dict create required false]
  dict set spec "--burn" [dict create required false]
  set args [argsInit $spec $opts]
  set burnValue [argsValue $args "--burn" ""]
  set epoch [argsValue $args "--epoch" [getCurrentEpoch]]
  astor [list --pay2pkh --from Percy --to Owner --value all]
  astor [list --pay2pkh --from Mary --to Owner --value all]
  astor [list --pay2pkh --from Shelley --to Owner --value all]
  astor [list --script withdraw --from Owner --epoch $epoch]
  if {$burnValue != ""} { astor [list --script burn --from Owner --value $burnValue] }
  astor [list --show all]
}

proc astorRun {opts} {
  dict set spec "--run" [dict create required true]
  set args [argsInit $spec $opts]
  set run [argsValue $args "--run" "workflow"]
  switch $run {
    workflow  { astorRunWorkflow $opts }
    tests     { astorRunTests $opts }
    default       { logError "Invalid command: $run"; astorHelp }
  }
}

proc astorRunWorkflow {opts} {
  global swaps
  logInfo [getSectionHeader "astor $opts"]
  dict set spec "--epoch" [dict create required false]
  set args [argsInit $spec $opts]
  set epoch [argsValue $args "--epoch" [getCurrentEpoch]]

  for {set i 0} {$i < $swaps} {incr i} {
    astor [list --pay2pkh --from Shelley --to Percy --value "10 Astor$epoch"]
    astor [list --script swap --from Percy --to Shelley --value "10 Astor$epoch"]
  }
  astor [list --show all]
}

# Astor Main Entry #############################################################
#

# Mainnet Workflow
#
# astor --script mint --from Owner --value '1000 Astor296'
# astor --pay2script --from Owner --value '10 Ada' --epoch 296
# astor --pay2pkh --from Owner --to Percy --value '10 Ada 10 Ada'
# astor --pay2pkh --from Owner --to addr1... --value '5 Astor296'
# astor --script swap --from Percy --to Owner --value '5 Astor296'
# astor --reset --epoch 296
# astor --script burn --from Owner --value '1000 Astor296'

# MintTokens: 1767771
# PayScript:   173245
# TokenSwap:  1161481
# Withdraw:    543353
# -------------------
#             2301052

set secBefore [clock seconds]

if {[llength $argv] == 0} {
  astorHelp
  return
}

# Always query the protocol parameters
cliQueryProtocolParameters

switch [lindex $argv 0] {
  --prepare     { astorPrepare $argv }
  --proxy       { astorProxy $argv }
  --run         { astorRun $argv }
  --help        { astorHelp $argv }
  default       { astor $argv }
}

puts ""
set secDiff [expr {[clock seconds] - $secBefore}]
puts "Done in [clock format $secDiff -format {%M:%S}]"
