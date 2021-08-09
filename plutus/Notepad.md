## Setup Dev Environment

### Install Nix

The quickest way to install Nix is to open a terminal and run the following command

```
curl -L "https://nixos.org/nix/install" | sh

# Configure Nix to use the binary cache from IOHK
sudo mkdir /etc/nix
cat << EOF | sudo tee /etc/nix/nix.conf
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
substituters = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org
EOF
```

### Install Cabal + GHC

https://www.haskell.org/ghcup

```
# Install ghcup
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# List installed components
ghcup list
```

### Haskell Language Server

* HLS may continously crash with plutus pioneer program
* Use cabal-3.4.0.0 / ghc-8.10.4
* Complile hls with `executable-dynamic: True`

```
vi ~/.cabal/config
executable-dynamic: True

cabal clean # just to be sure
cabal build exe:haskell-language-server-wrapper
cabal install exe:haskell-language-server-wrapper --overwrite-policy=always
```

### Compile the Sources

```
# Checkout Plutus version referenced by Alonzo-testnet
cd .../plutus; git checkout 826c2514a40e962c2e4d56ce912803a434cc28fe

# Start a Nix shell
nix-shell

# Change to .../Alonzo-testnet/resources/plutus-sources
cd .../plutus-sources

# Build the sources
cabal build plutus-helloworld

# Generate the HLS cradle
cabal install implicit-hie
gen-hie > hie.yaml

# The above should work. Now try a HLS build, like an IDE would
haskell-language-server-wrapper plutus-helloworld
```

### Atom Haskell

Run Atom from the Nix shell

https://atom-haskell.github.io

```
apm install language-haskell ide-haskell ide-haskell-cabal ide-haskell-hls
```

<!--
  Run the Plutus Playground ===========================================================================================
-->

## Run the Plutus Playground

```
# Checkout the Plutus version referencenced in cabal.project
cd ~/git/plutus \
  && git checkout ae35c4b8fe66dd626679bd2951bd72190e09a123 \
  && nix-shell

# Start the Plutus Playground Server
[nix-shell] cd plutus-playground-client && plutus-playground-server

# Start the Plutus Playground Client in another nix-shell
[nix-shell] cd plutus-playground-client && npm run start

https://localhost:8009
```

<!--
  Quickcheck Relay Node on the Testnet ================================================================================
-->

## Quickcheck Relay Node on the Testnet

### Using inputoutput/cardano-node

```
docker rm -f alonzo-relay
docker run --detach \
  --name=alonzo-relay \
  -p 3001:3001 \
  -e NETWORK=alonzo-purple \
  -v alonzo-data:/data/db \
  inputoutput/cardano-node:dev

docker logs -f alonzo-relay
```

### Using nessusio/cardano-node

```
docker run --detach \
    --name=alonzo-relay \
    -p 3001:3001 \
    -e CARDANO_NETWORK=alonzo-purple \
    -v alonzo-data:/opt/cardano/data \
    -v node-ipc:/opt/cardano/ipc \
    nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} run

docker logs -f alonzo-relay

docker exec -it alonzo-relay gLiveView
```

<!--
  Setup the Relay & Block Producer ====================================================================================
-->

## Get protocol parameters

```
alias cardano-cli="docker run -it --rm \
  -v ~/cardano:/var/cardano/local \
  -v node-ipc:/opt/cardano/ipc \
  nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} cardano-cli"

cardano-cli query protocol-parameters \
  --out-file /var/cardano/local/scratch/protocol.json \
  --testnet-magic 8
```

## Creating keys and addresses

https://github.com/input-output-hk/cardano-node/blob/master/doc/stake-pool-operations/keys_and_addresses.md

```
OWNER="acc0"

# Payment keys
cardano-cli address key-gen \
  --verification-key-file /var/cardano/local/keys/alonzo/${OWNER}/payment.vkey \
  --signing-key-file /var/cardano/local/keys/alonzo/${OWNER}/payment.skey \
&& cardano-cli stake-address key-gen \
  --verification-key-file /var/cardano/local/keys/alonzo/${OWNER}/stake.vkey \
  --signing-key-file /var/cardano/local/keys/alonzo/${OWNER}/stake.skey \
&& cardano-cli address build \
  --payment-verification-key-file /var/cardano/local/keys/alonzo/${OWNER}/payment.vkey \
  --stake-verification-key-file /var/cardano/local/keys/alonzo/${OWNER}/stake.vkey \
  --out-file /var/cardano/local/keys/alonzo/${OWNER}/payment.addr \
  --testnet-magic 8 \
&& cardano-cli stake-address build \
  --stake-verification-key-file /var/cardano/local/keys/alonzo/${OWNER}/stake.vkey \
  --out-file /var/cardano/local/keys/alonzo/${OWNER}/stake.addr \
  --testnet-magic 8
```

## Get funds from the Faucet

```
PAYMENT_ADDR=$(cat ~/cardano/keys/alonzo/${OWNER}/payment.addr)
echo $PAYMENT_ADDR

API_KEY="xxxxxx"
for i in 0 1 2 3 4 5 6 7 8 9; do
  curl -sX POST "https://faucet.alonzo-purple.dev.cardano.org/send-money/${PAYMENT_ADDR}?apiKey=${API_KEY}" | jq .;
  sleep 10;
done
```

## Register stake address on the blockchain

```
PAYMENT_ADDR=`cat ~/cardano/keys/alonzo/${OWNER}/payment.addr`
echo "${OWNER}: $PAYMENT_ADDR"

# Query UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic 8

cardano-cli stake-address registration-certificate \
  --stake-verification-key-file /var/cardano/local/keys/alonzo/${OWNER}/stake.vkey \
  --out-file /var/cardano/local/scratch/stake.cert

cat ~/cardano/scratch/protocol.json | grep stakeAddressDeposit

DEPOSIT=2000000
FEES_LVC=200000
REFUND_LVC=$((1000000000000 - $DEPOSIT - $FEES_LVC))
echo "$REFUND_LVC Lovelace"

# Build raw Tx
cardano-cli transaction build-raw \
  --tx-in "02906667cd499857b9c642dec50d1f2e44e9ff7592b1d6103d61e19fcd8c5bb6#0" \
  --tx-out $PAYMENT_ADDR+$REFUND_LVC \
  --fee $FEES_LVC \
  --certificate-file /var/cardano/local/scratch/stake.cert \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/${OWNER}/payment.skey \
  --signing-key-file /var/cardano/local/keys/alonzo/${OWNER}/stake.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8 \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
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
docker exec -it alonzo-relay ls -l /opt/cardano/config
docker cp alonzo-relay:/opt/cardano/config/alonzo-purple-shelley-genesis.json ~/cardano/scratch/

slotsPerKESPeriod=$(cat ~/cardano/scratch/alonzo-purple-shelley-genesis.json | jq ".slotsPerKESPeriod") \
&& slotNumber=$(cardano-cli query tip --testnet-magic 8 | jq ".slot") \
&& kesPeriod=$(expr $slotNumber / $slotsPerKESPeriod) \
&& echo "$slotNumber / $slotsPerKESPeriod => $kesPeriod"

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
  --staking-verification-key-file /var/cardano/local/keys/alonzo/${OWNER}/stake.vkey \
  --out-file /var/cardano/local/keys/alonzo/${OWNER}/delegation.cert
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
  --pool-pledge 1000000000 \
  --pool-cost 340000000 \
  --pool-margin 0 \
  --pool-reward-account-verification-key-file /var/cardano/local/keys/alonzo/${OWNER}/stake.vkey \
  --pool-owner-stake-verification-key-file /var/cardano/local/keys/alonzo/${OWNER}/stake.vkey \
  --single-host-pool-relay relay02.astorpool.net \
  --pool-relay-port 3001 \
  --metadata-url http://astorpool.net/astorpool.json \
  --metadata-hash "76a149ddde63485614d8c55cc86bd18dc6cd7f66a3d42dfdb27230ccd396840c" \
  --out-file /var/cardano/local/scratch/pool-registration.cert \
  --testnet-magic 8

cat ~/cardano/scratch/pool-registration.cert
```

### Create the pool registration Tx

```
PAYMENT_ADDR=`cat ~/cardano/keys/alonzo/${OWNER}/payment.addr`
echo "${OWNER}: $PAYMENT_ADDR"

# Query UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic 8

cat ~/cardano/scratch/protocol.json | grep stakePoolDeposit

DEPOSIT=500000000
FEES_LVC=200000
REFUND_LVC=$((999997800000 - $DEPOSIT - $FEES_LVC))
echo "$REFUND_LVC Lovelace"

# Build, sign and submit the Tx
cardano-cli transaction build-raw \
  --tx-in "623459b2e4b0031791190ff58ebac0372db231d5abac070adb6b20522dc7d353#0" \
  --tx-out $PAYMENT_ADDR+$REFUND_LVC \
  --fee $FEES_LVC \
  --certificate-file /var/cardano/local/scratch/pool-registration.cert \
  --certificate-file /var/cardano/local/keys/alonzo/${OWNER}/delegation.cert \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/${OWNER}/payment.skey \
  --signing-key-file /var/cardano/local/keys/alonzo/${OWNER}/stake.skey \
  --signing-key-file /var/cardano/local/keys/alonzo/pool/cold.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8 \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

## Verify that your stake pool registration was successful

```
# Query UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic 8

POOLID=$(cardano-cli stake-pool id \
  --cold-verification-key-file /var/cardano/local/keys/alonzo/pool/cold.vkey) \
&& echo "POOLID=${POOLID}"
```

## Query owner account

```
PAYMENT_ADDR0=$(cat ~/cardano/keys/alonzo/acc0/payment.addr)
STAKE_ADDR0=$(cat ~/cardano/keys/alonzo/acc0/stake.addr)
echo "${STAKE_ADDR0} => ${PAYMENT_ADDR0}"

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR0 \
  --testnet-magic 8

# Query rewards
cardano-cli query stake-address-info \
    --address $STAKE_ADDR0 \
    --testnet-magic 8
```

<!--
  Run the Relay Node ==================================================================================================
-->

## Run the Relay Node

```
RELAY_IP="relay02.astorpool.net"
BPROD_IP="xxx.domain.net"

# Setup the Producer topology
# The Producer connects to the Relay (only)

cat << EOF > ~/cardano/config/alonzo-relay-topology.json
{
  "Producers": [
    {
      "addr": "relays.alonzo-purple.dev.cardano.org",
      "port": 3001,
      "valency": 1
    },
    {
      "addr": "${BPROD_IP}",
      "port": 3001,
      "valency": 1
    }
  ]
}
EOF

docker volume rm -f alonzo-relay-config
docker run --name=tmp -v alonzo-relay-config:/var/cardano/config centos
docker cp ~/cardano/config/alonzo-relay-topology.json tmp:/var/cardano/config/alonzo-purple-topology.json
docker rm -f tmp

# Run the Relay node

docker stop alonzo-relay
docker rm alonzo-relay

docker run --detach \
    --name=alonzo-relay \
    --hostname=alonzo-relay \
    --restart=always \
    -p 3001:3001 \
    -e CARDANO_NETWORK=alonzo-purple \
    -e CARDANO_UPDATE_TOPOLOGY=true \
    -e CARDANO_PUBLIC_IP="${RELAY_IP}" \
    -e CARDANO_CUSTOM_PEERS="${BPROD_IP}:3001" \
    -e CARDANO_TOPOLOGY="/var/cardano/config/alonzo-purple-topology.json" \
    -v alonzo-relay-config:/var/cardano/config  \
    -v alonzo-data:/opt/cardano/data \
    -v node-ipc:/opt/cardano/ipc \
    nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} run

docker logs -n=200 -f alonzo-relay

docker exec -it alonzo-relay gLiveView

docker exec -it alonzo-relay tail -n 12 /opt/cardano/logs/topologyUpdateResult
docker exec -it alonzo-relay cat /var/cardano/config/alonzo-purple-topology.json

docker exec -it alonzo-relay tail -n 80 -f /opt/cardano/logs/debug.log
docker exec -it alonzo-relay lnav /opt/cardano/logs/debug.log

# Access the EKG metric
docker exec -it alonzo-relay curl -H 'Accept: application/json' 127.0.0.1:12788 | jq

# Access the Prometheus metrics
docker exec -it alonzo-relay curl 127.0.0.1:12798/metrics | sort
```

<!--
  Run the Block Producer ==============================================================================================
-->

## Run the Block Producer

```
# Setup the Producer topology
# The Producer connects to the Relay (only)

cat << EOF > ~/cardano/config/alonzo-bprod-topology.json
{
  "Producers": [
    {
      "addr": "${RELAY_IP}",
      "port": 3001,
      "valency": 1
    }
  ]
}
EOF

docker volume rm -f alonzo-bprod-config
docker run --name=tmp -v alonzo-bprod-config:/var/cardano/config centos
docker cp ~/cardano/config/alonzo-bprod-topology.json tmp:/var/cardano/config/alonzo-purple-topology.json
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
    --restart=always \
    -p 3001:3001 \
    -e CARDANO_NETWORK=alonzo-purple \
    -e CARDANO_BLOCK_PRODUCER=true \
    -e CARDANO_TOPOLOGY="/var/cardano/config/alonzo-purple-topology.json" \
    -e CARDANO_SHELLEY_KES_KEY="/var/cardano/config/keys/kes.skey" \
    -e CARDANO_SHELLEY_VRF_KEY="/var/cardano/config/keys/vrf.skey" \
    -e CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE="/var/cardano/config/keys/node.cert" \
    -v alonzo-bprod-keys:/var/cardano/config/keys  \
    -v alonzo-bprod-config:/var/cardano/config  \
    -v alonzo-data:/opt/cardano/data \
    -v node-ipc:/opt/cardano/ipc \
    nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} run

docker logs -n=200 -f alonzo-bprod

docker exec -it alonzo-bprod gLiveView

docker exec -it alonzo-bprod tail -n 80 -f /opt/cardano/logs/debug.log
docker exec -it alonzo-bprod lnav /opt/cardano/logs/debug.log

# Access the EKG metric
docker exec -it bprod curl -H 'Accept: application/json' 127.0.0.1:12788 | jq

# Access the Prometheus metrics
docker exec -it bprod curl 127.0.0.1:12798/metrics | sort
```

## Query account balances

```
PAYMENT_ADDR=$(cat ~/cardano/keys/alonzo/${OWNER}/payment.addr)
STAKE_ADDR=$(cat ~/cardano/keys/alonzo/${OWNER}/stake.addr)
echo "${STAKE_ADDR} => ${PAYMENT_ADDR}"

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic 8

# Query rewards
cardano-cli query stake-address-info \
    --address $STAKE_ADDR \
    --testnet-magic 8
```
