#!/bin/bash
#
# 
# https://docs.cardano.org/projects/cardano-node/en/latest/getting-started/install.html

echo "#========================================================="
echo "# Get configuration files"
echo "#"

TARGET=~/cardano/config/mainnet-config.json
if [ ! -f "$TARGET" ]; then

  mkdir -p ~/cardano/config
  wget -qO ~/cardano/config/mainnet-config.json https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/mainnet-config.json
  
else

  echo "Nothing to do!"
fi

echo "#========================================================="
echo "# Payment and Stake ~/cardano/keys/addresses"
echo "#"

mkdir -p ~/cardano/keys/spo
mkdir -p ~/cardano/keys/pool

TARGET=keys/spo/payment.skey
if [ ! -f "$TARGET" ]; then
  cardano-cli address key-gen \
    --verification-key-file ~/cardano/keys/spo/payment.vkey \
    --signing-key-file ~/cardano/keys/spo/payment.skey
fi
  
TARGET=keys/spo/stake.skey
if [ ! -f "$TARGET" ]; then
  cardano-cli stake-address key-gen \
    --verification-key-file ~/cardano/keys/spo/stake.vkey \
    --signing-key-file ~/cardano/keys/spo/stake.skey
fi
  
TARGET=keys/spo/payment.addr
if [ ! -f "$TARGET" ]; then
  cardano-cli address build \
    --payment-verification-key-file ~/cardano/keys/spo/payment.vkey \
    --stake-verification-key-file ~/cardano/keys/spo/stake.vkey \
    --out-file ~/cardano/keys/spo/payment.addr \
    --mainnet
fi
  
TARGET=keys/spo/stake.addr
if [ ! -f "$TARGET" ]; then
  cardano-cli stake-address build \
    --stake-verification-key-file ~/cardano/keys/spo/stake.vkey \
    --out-file ~/cardano/keys/spo/stake.addr \
    --mainnet
fi
  
echo "#========================================================="
echo "# Stake Pool Keys"
echo "#"

TARGET=keys/pool/cold.skey
if [ ! -f "$TARGET" ]; then
cardano-cli node key-gen \
  --cold-verification-key-file ~/cardano/keys/pool/cold.vkey \
  --cold-signing-key-file ~/cardano/keys/pool/cold.skey \
  --operational-certificate-issue-counter-file ~/cardano/keys/pool/cold.counter
fi

TARGET=keys/pool/vrf.skey
if [ ! -f "$TARGET" ]; then
  cardano-cli node key-gen-VRF \
    --verification-key-file ~/cardano/keys/pool/vrf.vkey \
    --signing-key-file ~/cardano/keys/pool/vrf.skey
fi

TARGET=keys/pool/kes.skey
if [ ! -f "$TARGET" ]; then
  cardano-cli node key-gen-KES \
    --verification-key-file ~/cardano/keys/pool/kes.vkey \
    --signing-key-file ~/cardano/keys/pool/kes.skey  
fi

TARGET=keys/pool/payment.skey
if [ ! -f "$TARGET" ]; then
  cardano-cli address key-gen \
    --verification-key-file ~/cardano/keys/pool/payment.vkey \
    --signing-key-file ~/cardano/keys/pool/payment.skey
fi

TARGET=keys/pool/payment.addr
if [ ! -f "$TARGET" ]; then
  cardano-cli address build \
    --payment-verification-key-file ~/cardano/keys/pool/payment.vkey \
    --out-file ~/cardano/keys/pool/payment.addr \
    --mainnet
fi

echo "#========================================================="
echo "# Generate the Operational Certificate"
echo "#"

SLOTS_PER_KES_PERIOD=`cat config/mainnet-shelley-genesis.json | jq '.slotsPerKESPeriod'`
echo "SLOTS_PER_KES_PERIOD: $SLOTS_PER_KES_PERIOD"

SLOT_NO=17291241
#SLOT_NO=`cardano-cli query tip --mainnet | jq '.slotNo'`
echo "SLOT_NO: $SLOT_NO"

KES_PERIOD=`expr $SLOT_NO / $SLOTS_PER_KES_PERIOD`
echo "KES_PERIOD: $KES_PERIOD"

TARGET=keys/pool/node.cert
if [ ! -f "$TARGET" ]; then
  cardano-cli node issue-op-cert \
    --kes-verification-key-file ~/cardano/keys/pool/kes.vkey \
    --cold-signing-key-file ~/cardano/keys/pool/cold.skey \
    --operational-certificate-issue-counter ~/cardano/keys/pool/cold.counter \
    --kes-period $KES_PERIOD \
    --out-file ~/cardano/keys/pool/node.cert
fi

echo ""
echo "#========================================================="

SPO_PAYMENT_ADDR=`cat ~/cardano/keys/spo/payment.addr`
POOL_PAYMENT_ADDR=`cat ~/cardano/keys/pool/payment.addr`

echo "SPO_PAYMENT_ADDR: $SPO_PAYMENT_ADDR"
echo "POOL_PAYMENT_ADDR: $POOL_PAYMENT_ADDR"

