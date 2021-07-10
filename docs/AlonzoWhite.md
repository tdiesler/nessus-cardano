## Running a Relay Node on the Testnet

```
docker rm -f alonzo-relay
docker run --detach \
    --name=alonzo-relay \
    -p 3001:3001 \
    -e CARDANO_NETWORK=alonzo-white \
    -v alonzo-data:/opt/cardano/data \
    -v alonzo-ipc:/opt/cardano/ipc \
    nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} run

docker logs -f alonzo-relay

docker exec -it alonzo-relay gLiveView
```

## Get protocol parameters

```
alias cardano-cli="docker run -it --rm \
  -v ~/cardano:/var/cardano/local \
  -v alonzo-ipc:/opt/cardano/ipc \
  nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} cardano-cli"

cardano-cli query protocol-parameters \
  --out-file /var/cardano/local/scratch/protocol.json \
  --testnet-magic 7
```

## Creating keys and addresses

https://github.com/input-output-hk/cardano-node/blob/master/doc/stake-pool-operations/keys_and_addresses.md

```
OWNER="owner"

# Payment keys
cardano-cli address key-gen \
  --verification-key-file /var/cardano/local/keys/alonzo/${OWNER}/payment.vkey \
  --signing-key-file /var/cardano/local/keys/alonzo/${OWNER}/payment.skey

# Stake keys
cardano-cli stake-address key-gen \
  --verification-key-file /var/cardano/local/keys/alonzo/${OWNER}/stake.vkey \
  --signing-key-file /var/cardano/local/keys/alonzo/${OWNER}/stake.skey  

# Payment address
cardano-cli address build \
  --payment-verification-key-file /var/cardano/local/keys/alonzo/${OWNER}/payment.vkey \
  --stake-verification-key-file /var/cardano/local/keys/alonzo/${OWNER}/stake.vkey \
  --out-file /var/cardano/local/keys/alonzo/${OWNER}/payment.addr \
  --testnet-magic 7

# Stake address
cardano-cli stake-address build \
  --stake-verification-key-file /var/cardano/local/keys/alonzo/${OWNER}/stake.vkey \
  --out-file /var/cardano/local/keys/alonzo/${OWNER}/stake.addr \
  --testnet-magic 7
```

## Get funds from the Faucet

```
PAYMENT_ADDR=$(cat ~/cardano/keys/alonzo/${OWNER}/payment.addr)
echo $PAYMENT_ADDR

API_KEY="xxxxxx"
curl -v -XPOST "https://faucet.alonzo-white.dev.cardano.org/send-money/${PAYMENT_ADDR}?apiKey=${API_KEY}"
```

## Register stake address on the blockchain

```
PAYMENT_ADDR=`cat ~/cardano/keys/alonzo/$OWNER/payment.addr`
echo "fees: $PAYMENT_ADDR"

# Query UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic 7

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1ec0e64a7cbd9ccbbf3c3b56fc141c3972f0f4756130f0287fa8dae449f52ce0     0        999499811883 lovelace

TX_IN="1ec0e64a7cbd9ccbbf3c3b56fc141c3972f0f4756130f0287fa8dae449f52ce0#0"

UTOX_LVC=999499811883

cardano-cli stake-address registration-certificate \
  --stake-verification-key-file /var/cardano/local/keys/alonzo/${OWNER}/stake.vkey \
  --out-file /var/cardano/local/scratch/stake.cert

cardano-cli transaction build-raw \
  --tx-in ${TX_IN} \
  --tx-out ${PAYMENT_ADDR}+0 \
  --invalid-hereafter 0 \
  --fee 0 \
  --certificate-file /var/cardano/local/scratch/stake.cert \
  --out-file /var/cardano/local/scratch/tx.draft

cardano-cli transaction calculate-min-fee \
  --protocol-params-file /var/cardano/local/scratch/protocol.json \
  --tx-body-file /var/cardano/local/scratch/tx.draft \
  --tx-in-count 1 \
  --tx-out-count 1 \
  --witness-count 2 \
  --testnet-magic 7

> 184817 Lovelace

cat ~/cardano/scratch/protocol.json | grep stakeAddressDeposit

DEPOSIT=2000000
FEES_LVC=184817
REFUND_LVC=`expr $UTOX_LVC - $DEPOSIT - $FEES_LVC`
echo "$REFUND_LVC Lovelace"

SLOT=`cardano-cli query tip --testnet-magic 7 | jq -c | jq ".slot"` \
  && TTL=`expr $SLOT + 3600` \
  && echo -e "Slot: $SLOT\nTTL:  $TTL"

# Build raw Tx
cardano-cli transaction build-raw \
  --tx-in $TX_IN \
  --tx-out $PAYMENT_ADDR+$REFUND_LVC \
  --ttl $TTL \
  --fee $FEES_LVC \
  --certificate-file /var/cardano/local/scratch/stake.cert \
  --out-file /var/cardano/local/scratch/tx.raw

cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/$OWNER/payment.skey \
  --signing-key-file /var/cardano/local/keys/alonzo/$OWNER/stake.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 7

cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 7
```

## Generate Pool Keys and Certificates

```
cardano-cli node key-gen \
  --cold-verification-key-file /var/cardano/local/keys/alonzo/pool/cold.vkey \
  --cold-signing-key-file /var/cardano/local/keys/alonzo/pool/cold.skey \
  --operational-certificate-issue-counter-file /var/cardano/local/keys/alonzo/pool/cold.counter

cardano-cli node key-gen-VRF \
  --verification-key-file /var/cardano/local/keys/alonzo/pool/vrf.vkey \
  --signing-key-file /var/cardano/local/keys/alonzo/pool/vrf.skey

cardano-cli node key-gen-KES \
  --verification-key-file /var/cardano/local/keys/alonzo/pool/kes.vkey \
  --signing-key-file /var/cardano/local/keys/alonzo/pool/kes.skey
```

### Calculate KES period

```
docker cp alonzo-relay:/opt/cardano/config/mainnet-shelley-genesis.json ~/cardano/scratch/
slotsPerKESPeriod=$(cat ~/cardano/scratch/mainnet-shelley-genesis.json | jq ".slotsPerKESPeriod")

slotNumber=$(cardano-cli query tip --testnet-magic 7 | jq ".slot")
kesPeriod=$(expr $slotNumber / $slotsPerKESPeriod)

echo "$slotNumber / $slotsPerKESPeriod => $kesPeriod"

cardano-cli node issue-op-cert \
  --kes-verification-key-file /var/cardano/local/keys/alonzo/pool/kes.vkey \
  --cold-signing-key-file /var/cardano/local/keys/alonzo/pool/cold.skey \
  --operational-certificate-issue-counter /var/cardano/local/keys/alonzo/pool/cold.counter \
  --kes-period $kesPeriod \
  --out-file /var/cardano/local/keys/alonzo/pool/node.cert
```

## Create delegation certificate

To honor your pledge, create a delegation certificate:

```
cardano-cli stake-address delegation-certificate \
  --stake-pool-verification-key-file /var/cardano/local/keys/alonzo/pool/cold.vkey \
  --staking-verification-key-file /var/cardano/local/keys/alonzo/$OWNER/stake.vkey \
  --out-file /var/cardano/local/keys/alonzo/$OWNER/delegation.cert
```

## Generate Stake Pool Registration Certificate

```
# Get the hash of your metadata JSON file

curl -so ~/cardano/scratch/astorpool.json http://astorpool.net/astorpool.json \
  && cat ~/cardano/scratch/astorpool.json

cardano-cli stake-pool metadata-hash \
  --pool-metadata-file /var/cardano/local/scratch/astorpool.json

cardano-cli stake-pool registration-certificate \
  --cold-verification-key-file /var/cardano/local/keys/alonzo/pool/cold.vkey \
  --vrf-verification-key-file /var/cardano/local/keys/alonzo/pool/vrf.vkey \
  --pool-pledge 200000000000 \
  --pool-cost 340000000 \
  --pool-margin 0.01 \
  --pool-reward-account-verification-key-file /var/cardano/local/keys/alonzo/$OWNER/stake.vkey \
  --pool-owner-stake-verification-key-file /var/cardano/local/keys/alonzo/$OWNER/stake.vkey \
  --single-host-pool-relay relay01.astorpool.net \
  --pool-relay-port 3001 \
  --metadata-url http://astorpool.net/astorpool.json \
  --metadata-hash "76a149ddde63485614d8c55cc86bd18dc6cd7f66a3d42dfdb27230ccd396840c" \
  --out-file /var/cardano/local/scratch/pool-registration.cert \
  --testnet-magic 7

cat ~/cardano/scratch/pool-registration.cert
```

### Create the pool registration Tx

```
PAYMENT_ADDR=`cat ~/cardano/keys/alonzo/$OWNER/payment.addr`
echo "fees: $PAYMENT_ADDR"

# Query UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic 7

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
383dcb445833c39c96b570c47752587e0c0d55f7f05515fd2ddb57e588a76acf     0        999497627066 lovelace

TX_IN="383dcb445833c39c96b570c47752587e0c0d55f7f05515fd2ddb57e588a76acf#0"

UTOX_LVC=999497627066

# Draft
cardano-cli transaction build-raw \
  --tx-in $TX_IN \
  --tx-out $PAYMENT_ADDR+0 \
  --ttl 0 \
  --fee 0 \
  --certificate-file /var/cardano/local/scratch/pool-registration.cert \
  --certificate-file /var/cardano/local/keys/alonzo/$OWNER/delegation.cert \
  --out-file /var/cardano/local/scratch/tx.draft

cardano-cli transaction calculate-min-fee \
  --tx-body-file /var/cardano/local/scratch/tx.draft \
  --protocol-params-file /var/cardano/local/scratch/protocol.json \
  --tx-in-count 1 \
  --tx-out-count 1 \
  --witness-count 1 \
  --testnet-magic 7

> 184817 Lovelace

cat ~/cardano/scratch/protocol.json | grep stakePoolDeposit

DEPOSIT=0
FEES_LVC=188117
REFUND_LVC=`expr $UTOX_LVC - $DEPOSIT - $FEES_LVC`
echo "$REFUND_LVC Lovelace"

SLOT=`cardano-cli query tip --testnet-magic 7 | jq -c | jq ".slot"` \
  && TTL=`expr $SLOT + 3600` \
  && echo -e "Slot: $SLOT\nTTL:  $TTL"

# Build raw Tx
cardano-cli transaction build-raw \
  --tx-in $TX_IN \
  --tx-out $PAYMENT_ADDR+$REFUND_LVC \
  --ttl $TTL \
  --fee $FEES_LVC \
  --certificate-file /var/cardano/local/scratch/pool-registration.cert \
  --certificate-file /var/cardano/local/keys/alonzo/$OWNER/delegation.cert \
  --out-file /var/cardano/local/scratch/tx.raw

```

### Sign and submit the transaction

```
cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/$OWNER/payment.skey \
  --signing-key-file /var/cardano/local/keys/alonzo/$OWNER/stake.skey \
  --signing-key-file /var/cardano/local/keys/alonzo/pool/cold.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 7

cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 7
```

## Verify that your stake pool registration was successful

```
# Query UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic 7

POOLID=$(cardano-cli stake-pool id \
  --cold-verification-key-file /var/cardano/local/keys/alonzo/pool/cold.vkey)

echo ${POOLID}
```

## Run a few tests

```
# Bech32 decoder https://runkit.com/60e96ea11daf1b001dc6929e/60e96eb2e59fda001a769b3c
POOL_HASH="0c11b7ef186952d83aaa5a15ab67794f9dde17a014bbddc4046d8c84"
curl -s https://smash.cardano-mainnet.iohk.io/api/v1/errors/${POOL_HASH}
```

## Run the Relay Node

```
# Setup the Producer topology
# The Producer connects to the Relay (only)

cat << EOF > ~/cardano/config/alonzo-relay-topology.json
{
  "Producers": [
    {
      "addr": "relays.alonzo-white.dev.cardano.org",
      "port": 3001,
      "valency": 1
    },
    {
      "addr": "alonzo-bprod.domain.net",
      "port": 3001,
      "valency": 1
    }
  ]
}
EOF

docker volume rm -f alonzo-relay-config
docker run --name=tmp -v alonzo-relay-config:/var/cardano/config centos
docker cp ~/cardano/config/alonzo-relay-topology.json tmp:/var/cardano/config/alonzo-white-topology.json
docker rm -f tmp

# Run the Relay node

docker stop alonzo-relay
docker rm alonzo-relay

docker run --detach \
    --name=alonzo-relay \
    --hostname=alonzo-relay \
    --restart=always \
    -p 3001:3001 \
    -e CARDANO_NETWORK=alonzo-white \
    -e CARDANO_UPDATE_TOPOLOGY=true \
    -e CARDANO_PUBLIC_IP="xxx.xxx.xxx.xxx" \
    -e CARDANO_CUSTOM_PEERS="alonzo-bprod.domain.net:3001" \
    -e CARDANO_TOPOLOGY="/var/cardano/config/alonzo-white-topology.json" \
    -v alonzo-relay-config:/var/cardano/config  \
    -v alonzo-data:/opt/cardano/data \
    -v alonzo-ipc:/opt/cardano/ipc \
    nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} run

docker logs -n=200 -f alonzo-relay

docker exec -it alonzo-relay gLiveView
docker exec -it alonzo-relay tail -n 12 /opt/cardano/logs/topologyUpdateResult
docker exec -it alonzo-relay cat /var/cardano/config/alonzo-white-topology.json

# Access the EKG metric
docker exec -it alonzo-relay curl -H 'Accept: application/json' 127.0.0.1:12788 | jq

# Access the Prometheus metrics
docker exec -it alonzo-relay curl 127.0.0.1:12798/metrics | sort
```

## Run the Block Producer

```
# Setup the Producer topology
# The Producer connects to the Relay (only)

cat << EOF > ~/cardano/config/alonzo-bprod-topology.json
{
  "Producers": [
    {
      "addr": "xxx.xxx.xxx.xxx",
      "port": 3001,
      "valency": 1
    }
  ]
}
EOF

docker volume rm -f alonzo-bprod-config
docker run --name=tmp -v alonzo-bprod-config:/var/cardano/config centos
docker cp ~/cardano/config/alonzo-bprod-topology.json tmp:/var/cardano/config/alonzo-white-topology.json
docker rm -f tmp

# Setup Block Producer keys

chmod 600 ~/cardano/keys/alonzo/pool/*

docker volume rm -f alonzo-bprod-keys
docker run --name=tmp -v alonzo-bprod-keys:/var/cardano/config/keys centos
docker cp ~/cardano/keys/alonzo/pool/node.cert tmp:/var/cardano/config/keys
docker cp ~/cardano/keys/alonzo/pool/kes.skey tmp:/var/cardano/config/keys
docker cp ~/cardano/keys/alonzo/pool/vrf.skey tmp:/var/cardano/config/keys
docker rm -f tmp

docker run -it --rm \
  -v alonzo-bprod-keys:/var/cardano/config/keys \
  -v alonzo-bprod-config:/var/cardano/config \
  centos find /var/cardano/config -type f | sort

# Run the Producer node

docker stop alonzo-bprod
docker rm alonzo-bprod

docker run --detach \
    --name=alonzo-bprod \
    --hostname=alonzo-bprod \
    -p 3001:3001 \
    -e CARDANO_NETWORK=alonzo-white \
    -e CARDANO_BLOCK_PRODUCER=true \
    -e CARDANO_TOPOLOGY="/var/cardano/config/alonzo-white-topology.json" \
    -e CARDANO_SHELLEY_KES_KEY="/var/cardano/config/keys/kes.skey" \
    -e CARDANO_SHELLEY_VRF_KEY="/var/cardano/config/keys/vrf.skey" \
    -e CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE="/var/cardano/config/keys/node.cert" \
    -v alonzo-bprod-keys:/var/cardano/config/keys  \
    -v alonzo-bprod-config:/var/cardano/config  \
    -v alonzo-data:/opt/cardano/data \
    -v alonzo-ipc:/opt/cardano/ipc \
    nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} run

docker logs -n=200 -f alonzo-bprod

docker exec -it alonzo-bprod gLiveView

# Access the EKG metric
docker exec -it bprod curl -H 'Accept: application/json' 127.0.0.1:12788 | jq

# Access the Prometheus metrics
docker exec -it bprod curl 127.0.0.1:12798/metrics | sort
```
