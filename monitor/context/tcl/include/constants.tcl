
# Define global constants ######################################################
#

proc envvar {vname {default ""}} {
  set result $default
  catch { set result "$::env($vname)" }
  return $result
}

set BLOCKFROST_NETWORK [envvar "BLOCKFROST_NETWORK" "mainnet"]
set BLOCKFROST_API_URL [envvar "BLOCKFROST_API_URL" "https://cardano-$BLOCKFROST_NETWORK.blockfrost.io/api/v0"]
set BLOCKFROST_API_KEY [envvar "BLOCKFROST_API_KEY" ""]

set LEVEL_DEBUG 1
set LEVEL_INFO  2
set LEVEL_WARN  3
set LEVEL_ERROR 4

set LOG_LEVEL $LEVEL_INFO
