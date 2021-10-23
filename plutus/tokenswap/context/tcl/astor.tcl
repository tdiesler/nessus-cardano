#! /usr/local/bin/tclsh

set scripPath [file normalize [info script]]
set tcldir [file dirname $scripPath]

source $tcldir/include/essentials.tcl
source $tcldir/include/constants.tcl
source $tcldir/include/contract.tcl
source $tcldir/include/proxy.tcl
source $tcldir/include/tests.tcl
source $tcldir/include/utils.tcl
source $tcldir/include/wallet.tcl

# Astor Main Procs #############################################################
#

set swaps 4

proc astorPrepare {opts} {
  global swaps
  logInfo [getSectionHeader "astor $opts"]

  dict set spec "--epoch" [dict create required false]
  set args [argsInit $spec $opts]
  set epoch [argsValue $args "--epoch" [getCurrentEpoch]]

  set values "10 Ada 10 Ada"
  for {set i 0} {$i < $swaps} {incr i} {
    set values [concat $values "10 Astor$epoch"]
  }

  astor [list --reset --epoch $epoch]
  astor [list --pay2pkh --from Owner --to Shelley --value $values]
  astor [list --pay2pkh --from Owner --to Percy --value "10 Ada 10 Ada"]
  astor [list --show all]
}

proc astorRun {opts} {
  global swaps
  logInfo [getSectionHeader "astor $opts"]

  dict set spec "--epoch" [dict create required false]
  set args [argsInit $spec $opts]
  set epoch [argsValue $args "--epoch" [getCurrentEpoch]]

  astor [list --pay2script --from Owner --epoch $epoch --value "100 Ada" --ttl 0]

  for {set i 0} {$i < $swaps} {incr i} {
    astor [list --pay2pkh --from Shelley --to Percy --value "10 Astor$epoch"]
    astor [list --script swap --from Percy --to Shelley --value "10 Astor$epoch"]
  }
  astor [list --show all]
}

proc astorRunTests {opts} {

  logInfo [getSectionHeader "astor $opts"]

  dict set spec "--epoch" [dict create required false]
  set args [argsInit $spec $opts]
  set epoch [argsValue $args "--epoch" [getCurrentEpoch]]

  set values "10 Ada 10 Ada 10 Astor$epoch"

  astor [list --reset --epoch $epoch]
  astor [list --pay2pkh --from Owner --to Shelley --value $values]
  astor [list --pay2script --from Owner --epoch $epoch --value "100 Ada"]
  astor [list --show all]

  testUnauthorizedWithdraw $epoch

  testInvalidTokenSwaps 10 "Astor$epoch"

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
  --run         { astorRun $argv }
  --run-tests   { astorRunTests $argv }
  --proxy-run   { astorProxyRun $argv }
  --proxy-test  { astorProxyTest $argv }
  default       { astor $argv }
}

puts ""
set secDiff [expr {[clock seconds] - $secBefore}]
puts "Done in [clock format $secDiff -format {%M:%S}]"
