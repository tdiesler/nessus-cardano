
# Define global constants ######################################################
#

set MIN_COLLATERAL 2000000
set MIN_TOKEN_LOVELACE 1700000
set MIN_PLUTUS_FEES 1200000
set TOKEN_TTL_EPOCHS 18

set LOG_LEVEL $LEVEL_INFO

set CARDANO_DIR "$::env(HOME)/cardano"
set DOCKER_RUNTIME [file exists "/usr/local/bin/cardano-cli"]
if {$DOCKER_RUNTIME} {
  set CARDANO_DIR "/var/cardano/local"
}

set BLOCKFROST_NETWORK [envvar "BLOCKFROST_NETWORK" "testnet"]
set BLOCKFROST_API_URL [envvar "BLOCKFROST_API_URL" "https://cardano-$BLOCKFROST_NETWORK.blockfrost.io/api/v0"]
set BLOCKFROST_API_KEY [envvar "BLOCKFROST_API_KEY" ""]

set SCRATCH_DIR "$CARDANO_DIR/scratch"
set NETWORK_DIR "$CARDANO_DIR/$BLOCKFROST_NETWORK"
set SCRIPTS_DIR "$BLOCKFROST_NETWORK/scripts"
set KEYS_DIR "$NETWORK_DIR/keys"

if [isNetwork "testnet"] {
  set POLICY_ID [envvar "POLICY_ID" "891b79189cc2ad6175160655a0e6286036695af10d2511f77f966e5c"]
  set SCRIPT_ADDR [envvar "SCRIPT_ADDR" "addr_test1wzzwvjx9k5u7dyqmvze2ft77jjr797r279zyf6vq44zc65sndxe2y"]
  set TESTNET_MAGIC [envvar "TESTNET_MAGIC" "1097911063"]
} else { if [isNetwork "mainnet"] {
  set POLICY_ID [envvar "POLICY_ID" "3f997b68b1f491c7c2f10af4e2bf9566c5d25bd61df0343065d4fe1c"]
  set SCRIPT_ADDR [envvar "SCRIPT_ADDR" "addr1w9adgfaux7vjj948u5vfmszx6v3ylc9typjjrkke4n6alhcwu2j5z"]
} else {
  logError "Unsupported BLOCKFROST_NETWORK: $BLOCKFROST_NETWORK"
  exit 2
}}

if {$BLOCKFROST_API_KEY == ""} {
  logError "Cannot obtain BLOCKFROST_API_KEY"
  exit 2
}

if {$DOCKER_RUNTIME} {
  puts "BLOCKFROST_NETWORK=$BLOCKFROST_NETWORK"
  puts "BLOCKFROST_API_URL=$BLOCKFROST_API_URL"
  puts "BLOCKFROST_API_KEY=[string range $BLOCKFROST_API_KEY 0 15]..."
  puts "SCRATCH_DIR=$SCRATCH_DIR"
  puts "NETWORK_DIR=$NETWORK_DIR"
  puts "SCRIPTS_DIR=$SCRIPTS_DIR"
  puts "KEYS_DIR=$KEYS_DIR"
  puts "POLICY_ID=$POLICY_ID"
  puts "SCRIPT_ADDR=$SCRIPT_ADDR"
  puts "MIN_COLLATERAL=$MIN_COLLATERAL"
  puts "MIN_PLUTUS_FEES=$MIN_PLUTUS_FEES"
  puts "MIN_TOKEN_LOVELACE=$MIN_TOKEN_LOVELACE"
}
