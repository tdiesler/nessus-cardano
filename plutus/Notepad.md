# Setup Dev Environment

## Install Nix

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

## Install Cabal + GHC

https://www.haskell.org/ghcup

```
# Install ghcup
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# List installed components
ghcup list
```

## Haskell Language Server

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

## Compile the Sources

```
# Checkout the Plutus version referencenced in cabal.project
cd ~/git/plutus \
  && git checkout 8c83c4abe211b4bbcaca3cdf1b2c0e38d0eb683f \
  && nix-shell

# Start a Nix shell
nix-shell

# Change to plutus alonzo sources
cd ../nessus-cardano/plutus/alonzo/plutus-sources

# Build the sources
cabal clean
cabal update
cabal build plutus-helloworld

# Generate the HLS cradle
cabal install implicit-hie
gen-hie > hie.yaml

# The above should work. Now try a HLS build, like an IDE would
haskell-language-server-wrapper plutus-helloworld
```

## Atom Haskell

Run Atom from the Nix shell

https://atom-haskell.github.io

```
apm install language-haskell ide-haskell ide-haskell-cabal ide-haskell-hls
```

<!--
  Run the Plutus Playground ===========================================================================================
-->

# Run the Plutus Playground

```
# Checkout the Plutus version referencenced in cabal.project
cd ~/git/plutus \
  && git checkout 8c83c4abe211b4bbcaca3cdf1b2c0e38d0eb683f \
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

# Quickcheck Relay Node on the Testnet

## Using inputoutput/cardano-node

```
docker rm -f testrl
docker run --detach \
  --name=testrl \
  -p 3001:3001 \
  -e NETWORK=testnet \
  -v test-data:/data/db \
  inputoutput/cardano-node

docker logs -f testrl
```

## Using nessusio/cardano-node

```
docker run --detach \
    --name=testrl \
    -p 3001:3001 \
    -e CARDANO_NETWORK=testnet \
    -v test-data:/opt/cardano/data \
    -v node-ipc:/opt/cardano/ipc \
    nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} run

docker logs -f testrl

docker exec -it testrl gLiveView
```

<!--
  Setup the Relay & Block Producer ====================================================================================
-->

# Setup the Relay & Block Producer

## Get protocol parameters

```
alias cardano-cli="docker run -it --rm \
  -v ~/cardano:/var/cardano/local \
  -v node-ipc:/opt/cardano/ipc \
  nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} cardano-cli"

TESTNET_MAGIC=$(docker exec testrl cat /opt/cardano/data/protocolMagicId) \
  && echo "TESTNET_MAGIC=$TESTNET_MAGIC"

cardano-cli query protocol-parameters \
  --out-file /var/cardano/local/scratch/protocol.json \
  --testnet-magic $TESTNET_MAGIC
```

## Creating keys and addresses

https://github.com/input-output-hk/cardano-node/blob/master/doc/stake-pool-operations/keys_and_addresses.md

```
OWNER="acc0"

# Owner payment keys
mkdir -p ~/cardano/keys/testnet/${OWNER} \
&& cardano-cli address key-gen \
  --verification-key-file /var/cardano/local/keys/testnet/${OWNER}/payment.vkey \
  --signing-key-file /var/cardano/local/keys/testnet/${OWNER}/payment.skey \
&& cardano-cli stake-address key-gen \
  --verification-key-file /var/cardano/local/keys/testnet/${OWNER}/stake.vkey \
  --signing-key-file /var/cardano/local/keys/testnet/${OWNER}/stake.skey \
&& cardano-cli address build \
  --payment-verification-key-file /var/cardano/local/keys/testnet/${OWNER}/payment.vkey \
  --stake-verification-key-file /var/cardano/local/keys/testnet/${OWNER}/stake.vkey \
  --out-file /var/cardano/local/keys/testnet/${OWNER}/payment.addr \
  --testnet-magic $TESTNET_MAGIC \
&& cardano-cli stake-address build \
  --stake-verification-key-file /var/cardano/local/keys/testnet/${OWNER}/stake.vkey \
  --out-file /var/cardano/local/keys/testnet/${OWNER}/stake.addr \
  --testnet-magic $TESTNET_MAGIC \
&& cardano-cli address key-hash \
  --payment-verification-key-file /var/cardano/local/keys/testnet/${OWNER}/payment.vkey \
  --out-file /var/cardano/local/keys/testnet/${OWNER}/payment.pkh
&& echo "${OWNER}=$(cat ~/cardano/keys/testnet/${OWNER}/payment.addr)"

# User payment keys
for i in 1 2 3; do
  USER="acc$i"
  mkdir -p ~/cardano/keys/testnet/${USER} \
  && cardano-cli address key-gen \
    --verification-key-file /var/cardano/local/keys/testnet/${USER}/payment.vkey \
    --signing-key-file /var/cardano/local/keys/testnet/${USER}/payment.skey \
  && cardano-cli address build \
    --payment-verification-key-file /var/cardano/local/keys/testnet/${USER}/payment.vkey \
    --out-file /var/cardano/local/keys/testnet/${USER}/payment.addr \
    --testnet-magic $TESTNET_MAGIC \
  && cardano-cli address key-hash \
    --payment-verification-key-file /var/cardano/local/keys/testnet/${USER}/payment.vkey \
    --out-file /var/cardano/local/keys/testnet/${USER}/payment.pkh \
  && echo "${USER}=$(cat ~/cardano/keys/testnet/${USER}/payment.addr)"
done

# Zip the keys for secure storage
zip -re cardano-testnet-keys.zip ~/cardano/keys/testnet
```

## Get funds from the Faucet

https://testnets.cardano.org/en/testnets/cardano/tools/faucet/

```
PAYMENT_ADDR=$(cat ~/cardano/keys/testnet/${OWNER}/payment.addr) \
  && echo $PAYMENT_ADDR
```

## Register stake address on the blockchain

```
PAYMENT_ADDR=`cat ~/cardano/keys/testnet/${OWNER}/payment.addr`
echo "${OWNER}: $PAYMENT_ADDR"

# Query UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic $TESTNET_MAGIC

cardano-cli stake-address registration-certificate \
  --stake-verification-key-file /var/cardano/local/keys/testnet/${OWNER}/stake.vkey \
  --out-file /var/cardano/local/scratch/stake.cert

cat ~/cardano/scratch/protocol.json | grep stakeAddressDeposit

DEPOSIT=2000000
FEES_LVC=200000
REFUND_LVC=$((1000000000 - $DEPOSIT - $FEES_LVC))
echo "$REFUND_LVC Lovelace"

# Build raw Tx
cardano-cli transaction build-raw \
  --tx-in "626b5957d2ea9656c9959d2ae71dab58d8658535ff2a264cd03ce97e5387f75d#0" \
  --tx-out $PAYMENT_ADDR+$REFUND_LVC \
  --fee $FEES_LVC \
  --certificate-file /var/cardano/local/scratch/stake.cert \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/${OWNER}/payment.skey \
  --signing-key-file /var/cardano/local/keys/testnet/${OWNER}/stake.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

## Generate Pool Keys and Certificates

```
cardano-cli node key-gen \
  --cold-verification-key-file /var/cardano/local/keys/testnet/pool/cold.vkey \
  --cold-signing-key-file /var/cardano/local/keys/testnet/pool/cold.skey \
  --operational-certificate-issue-counter-file /var/cardano/local/keys/testnet/pool/cold.counter \
&& cardano-cli node key-gen-VRF \
  --verification-key-file /var/cardano/local/keys/testnet/pool/vrf.vkey \
  --signing-key-file /var/cardano/local/keys/testnet/pool/vrf.skey \
&& cardano-cli node key-gen-KES \
  --verification-key-file /var/cardano/local/keys/testnet/pool/kes.vkey \
  --signing-key-file /var/cardano/local/keys/testnet/pool/kes.skey
```

## Calculate KES period

```
docker exec -it testrl ls -l /opt/cardano/config
docker cp testrl:/opt/cardano/config/testnet-shelley-genesis.json ~/cardano/scratch/

slotsPerKESPeriod=$(cat ~/cardano/scratch/testnet-shelley-genesis.json | jq ".slotsPerKESPeriod") \
&& slotNumber=$(cardano-cli query tip --testnet-magic $TESTNET_MAGIC | jq ".slot") \
&& kesPeriod=$(expr $slotNumber / $slotsPerKESPeriod) \
&& echo "$slotNumber / $slotsPerKESPeriod => $kesPeriod"

cardano-cli node issue-op-cert \
  --kes-verification-key-file /var/cardano/local/keys/testnet/pool/kes.vkey \
  --cold-signing-key-file /var/cardano/local/keys/testnet/pool/cold.skey \
  --operational-certificate-issue-counter /var/cardano/local/keys/testnet/pool/cold.counter \
  --kes-period $kesPeriod \
  --out-file /var/cardano/local/keys/testnet/pool/node.cert
```

## Create delegation certificate

To honor your pledge, create a delegation certificate:

```
cardano-cli stake-address delegation-certificate \
  --stake-pool-verification-key-file /var/cardano/local/keys/testnet/pool/cold.vkey \
  --staking-verification-key-file /var/cardano/local/keys/testnet/${OWNER}/stake.vkey \
  --out-file /var/cardano/local/keys/testnet/${OWNER}/delegation.cert
```

## Generate Stake Pool Registration Certificate

```
# Get the hash of your metadata JSON file

curl -so ~/cardano/scratch/astorpool.json https://astorpool.net/astorpool.json \
  && cat ~/cardano/scratch/astorpool.json

cardano-cli stake-pool metadata-hash \
  --pool-metadata-file /var/cardano/local/scratch/astorpool.json

cardano-cli stake-pool registration-certificate \
  --cold-verification-key-file /var/cardano/local/keys/testnet/pool/cold.vkey \
  --vrf-verification-key-file /var/cardano/local/keys/testnet/pool/vrf.vkey \
  --pool-pledge 100000000 \
  --pool-cost 340000000 \
  --pool-margin 0 \
  --pool-reward-account-verification-key-file /var/cardano/local/keys/testnet/${OWNER}/stake.vkey \
  --pool-owner-stake-verification-key-file /var/cardano/local/keys/testnet/${OWNER}/stake.vkey \
  --single-host-pool-relay relay02.astorpool.net \
  --pool-relay-port 3001 \
  --metadata-url https://astorpool.net/astorpool.json \
  --metadata-hash "c189f05a46382086e09ebb51e84a4fad29d104416e61fe48f66d62cf2e0ec897" \
  --out-file /var/cardano/local/scratch/pool-registration.cert \
  --testnet-magic $TESTNET_MAGIC

cat ~/cardano/scratch/pool-registration.cert
```

## Create the pool registration Tx

```
PAYMENT_ADDR=`cat ~/cardano/keys/testnet/${OWNER}/payment.addr`
echo "${OWNER}: $PAYMENT_ADDR"

# Query UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic $TESTNET_MAGIC

cat ~/cardano/scratch/protocol.json | grep stakePoolDeposit

DEPOSIT=500000000
FEES_LVC=200000
REFUND_LVC=$((997800000 - $DEPOSIT - $FEES_LVC))
echo "$REFUND_LVC Lovelace"

# Build, sign and submit the Tx
cardano-cli transaction build-raw \
  --tx-in "0874482664ad2bc23c71ffa15e7cec38385dd8fa9f04176878df42ef5025ea4d#0" \
  --tx-out $PAYMENT_ADDR+$REFUND_LVC \
  --fee $FEES_LVC \
  --certificate-file /var/cardano/local/scratch/pool-registration.cert \
  --certificate-file /var/cardano/local/keys/testnet/${OWNER}/delegation.cert \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/${OWNER}/payment.skey \
  --signing-key-file /var/cardano/local/keys/testnet/${OWNER}/stake.skey \
  --signing-key-file /var/cardano/local/keys/testnet/pool/cold.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

## Verify that your stake pool registration was successful

```
# Query UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic $TESTNET_MAGIC

POOLID=$(cardano-cli stake-pool id \
  --cold-verification-key-file /var/cardano/local/keys/testnet/pool/cold.vkey) \
&& echo "POOLID=${POOLID}"
```

## Query owner account

```
PAYMENT_ADDR0=$(cat ~/cardano/keys/testnet/acc0/payment.addr)
STAKE_ADDR0=$(cat ~/cardano/keys/testnet/acc0/stake.addr)
echo "${STAKE_ADDR0} => ${PAYMENT_ADDR0}"

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR0 \
  --testnet-magic $TESTNET_MAGIC

# Query rewards
cardano-cli query stake-address-info \
    --address $STAKE_ADDR0 \
    --testnet-magic $TESTNET_MAGIC
```

<!--
  Run the Relay Node ==================================================================================================
-->

## Run the Relay Node

```
RELAY_IP="testrl01.astorpool.net"
BPROD_IP="xxx.domain.net"

# Setup the Producer topology
# The Producer connects to the Relay (only)

cat << EOF > ~/cardano/config/testnet-relay-topology.json
{
  "Producers": [
    {
      "addr": "relays-new.cardano-testnet.iohkdev.io",
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

docker volume rm -f testnet-relay-config
docker run --name=tmp -v testnet-relay-config:/var/cardano/config centos
docker cp ~/cardano/config/testnet-relay-topology.json tmp:/var/cardano/config/testnet-topology.json
docker rm -f tmp

# Run the Relay node

docker stop testrl
docker rm testrl

docker run --detach \
    --name=testrl \
    --hostname=testrl \
    --restart=always \
    -p 3001:3001 \
    -e CARDANO_NETWORK=testnet \
    -e CARDANO_UPDATE_TOPOLOGY=true \
    -e CARDANO_PUBLIC_IP="${RELAY_IP}" \
    -e CARDANO_CUSTOM_PEERS="${BPROD_IP}:3001" \
    -e CARDANO_TOPOLOGY="/var/cardano/config/testnet-topology.json" \
    -v testnet-relay-config:/var/cardano/config  \
    -v test-data:/opt/cardano/data \
    -v node-ipc:/opt/cardano/ipc \
    nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} run

docker logs -n=200 -f testrl

docker exec -it testrl gLiveView

docker exec -it testrl tail -n 12 /opt/cardano/logs/topologyUpdateResult
docker exec -it testrl cat /var/cardano/config/testnet-topology.json

docker exec -it testrl tail -n 80 -f /opt/cardano/logs/debug.log
docker exec -it testrl lnav /opt/cardano/logs/debug.log

# Access the EKG metric
docker exec -it testrl curl -H 'Accept: application/json' 127.0.0.1:12788 | jq

# Access the Prometheus metrics
docker exec -it testrl curl 127.0.0.1:12798/metrics | sort
```

<!--
  Run the Block Producer ==============================================================================================
-->

## Run the Block Producer

```
# Setup the Producer topology
# The Producer connects to the Relay (only)

cat << EOF > ~/cardano/config/testnet-bprod-topology.json
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

docker volume rm -f testnet-bprod-config
docker run --name=tmp -v testnet-bprod-config:/var/cardano/config centos
docker cp ~/cardano/config/testnet-bprod-topology.json tmp:/var/cardano/config/testnet-topology.json
docker rm -f tmp

# Setup Block Producer keys

chmod 600 ~/cardano/keys/testnet/pool/*

docker volume rm -f testnet-bprod-keys
docker run --name=tmp -v testnet-bprod-keys:/var/cardano/config/keys centos
docker cp ~/cardano/keys/testnet/pool/node.cert tmp:/var/cardano/config/keys
docker cp ~/cardano/keys/testnet/pool/kes.skey tmp:/var/cardano/config/keys
docker cp ~/cardano/keys/testnet/pool/vrf.skey tmp:/var/cardano/config/keys
docker rm -f tmp

docker run -it --rm \
  -v testnet-bprod-keys:/var/cardano/config/keys \
  -v testnet-bprod-config:/var/cardano/config \
  centos find /var/cardano/config -type f | sort

# Run the Producer node

docker stop testnet-bprod
docker rm testnet-bprod

docker run --detach \
    --name=testbp \
    --hostname=testbp \
    --restart=always \
    -p 3001:3001 \
    -e CARDANO_NETWORK=testnet \
    -e CARDANO_BLOCK_PRODUCER=true \
    -e CARDANO_TOPOLOGY="/var/cardano/config/testnet-topology.json" \
    -e CARDANO_SHELLEY_KES_KEY="/var/cardano/config/keys/kes.skey" \
    -e CARDANO_SHELLEY_VRF_KEY="/var/cardano/config/keys/vrf.skey" \
    -e CARDANO_SHELLEY_OPERATIONAL_CERTIFICATE="/var/cardano/config/keys/node.cert" \
    -v testnet-bprod-keys:/var/cardano/config/keys  \
    -v testnet-bprod-config:/var/cardano/config  \
    -v /mnt/disks/data00/testdat:/opt/cardano/data \
    -v node-ipc:/opt/cardano/ipc \
    nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} run

docker logs -n=200 -f testbp

docker exec -it testbp gLiveView

docker exec -it testbp tail -n 80 -f /opt/cardano/logs/debug.log
docker exec -it testbp lnav /opt/cardano/logs/debug.log

# Access the EKG metric
docker exec -it bprod curl -H 'Accept: application/json' 127.0.0.1:12788 | jq

# Access the Prometheus metrics
docker exec -it bprod curl 127.0.0.1:12798/metrics | sort
```

## Query account balances

```
PAYMENT_ADDR=$(cat ~/cardano/keys/testnet/${OWNER}/payment.addr)
STAKE_ADDR=$(cat ~/cardano/keys/testnet/${OWNER}/stake.addr)
echo "${STAKE_ADDR} => ${PAYMENT_ADDR}"

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic $TESTNET_MAGIC

# Query rewards
cardano-cli query stake-address-info \
    --address $STAKE_ADDR \
    --testnet-magic $TESTNET_MAGIC
```
