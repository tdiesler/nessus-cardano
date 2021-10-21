## Build the TokenSwap image

```
NETWORK=${BLOCKFROST_NETWORK:-testnet} \
&& OWNER_ADDR=$(cat ~/cardano/$NETWORK/keys/acc0/payment.addr) \
&& SHELLEY_ADDR=$(cat ~/cardano/$NETWORK/keys/acc1/payment.addr) \
&& MARY_ADDR=$(cat ~/cardano/$NETWORK/keys/acc2/payment.addr) \
&& PERCY_ADDR=$(cat ~/cardano/$NETWORK/keys/acc3/payment.addr)

if [ $NETWORK == "testnet" ]; then
  TESTNET_MAGIC=$(docker exec testrl cat /opt/cardano/data/protocolMagicId) \
  && NETWORK_OPTION="--testnet-magic $TESTNET_MAGIC" \
  && echo "NETWORK_OPTION=$NETWORK_OPTION"
elif [ $NETWORK == "mainnet" ]; then
  NETWORK_OPTION="--mainnet" \
  && echo "NETWORK_OPTION=$NETWORK_OPTION"
fi

cd ~/git/nessus-cardano/plutus/tokenswap \
  && cabal run swap-tokens swaptokens.plutus \
  && mv *.plutus ~/cardano/$NETWORK/scripts \
  && cardano-cli address build $NETWORK_OPTION \
    --payment-script-file /var/cardano/local/$NETWORK/scripts/swaptokens.plutus \
    --out-file /var/cardano/local/scratch/swaptokens.addr \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/swaptokens.addr) \
  && echo "SCRIPT_ADDR=\"${SCRIPT_ADDR}\""

ExBudget {exBudgetCPU = ExCPU 20870973, exBudgetMemory = ExMemory 70200}
SCRIPT_ADDR="addr_test1wzzpdrzjpyeyra9h4ymtlx7znsnc9ada08kzwjqqu5dyxvqup7fma"

docker build -t nessusio/tokenswap ./context
```

## Setup the config volume

```
docker volume rm -f tokenswap \
&& docker run --name=tmp -v tokenswap:/var/cardano/local centos \
  mkdir -p /var/cardano/local/mainnet \
  mkdir -p /var/cardano/local/mainnet/keys \
  mkdir -p /var/cardano/local/testnet \
  mkdir -p /var/cardano/local/testnet/keys \
  mkdir -p /var/cardano/local/scratch \
&& docker cp ~/cardano/mainnet/scripts tmp:/var/cardano/local/mainnet/scripts \
&& docker cp ~/cardano/mainnet/keys/acc3 tmp:/var/cardano/local/mainnet/keys \
&& docker cp ~/cardano/testnet/scripts tmp:/var/cardano/local/testnet/scripts \
&& docker cp ~/cardano/testnet/keys/acc3 tmp:/var/cardano/local/testnet/keys \
&& docker run -it --rm -v tokenswap:/var/cardano/local centos \
  find /var/cardano/local \
&& docker rm -f tmp

docker run --name=tmp -v tokenswap:/var/cardano/local centos \
&& docker cp tmp:/var/cardano/local ~/tokenswap
&& docker rm -f tmp
```

## Run Percy to Script

```
NETWORK="${BLOCKFROST_NETWORK:-testnet}" \
&& docker build -t nessusio/tokenswap ./context \
&& docker rm -f tokenswap \
&& docker run --detach \
  --name tokenswap \
  --restart=always \
  -e BLOCKFROST_NETWORK="${NETWORK}" \
  -e BLOCKFROST_API_KEY="$BLOCKFROST_API_KEY" \
  -e PROXY_FROM_NAME="Percy" \
  -e PROXY_FROM_ADDR="$(cat ~/cardano/${NETWORK}/keys/acc3/payment.addr)" \
  -e PROXY_FROM_SKEY="${NETWORK}/keys/acc3/payment.skey" \
  -v tokenswap:/var/cardano/local \
  -v node-ipc:/opt/cardano/ipc \
  nessusio/tokenswap --proxy-run --endless true

docker logs -f tokenswap
```
