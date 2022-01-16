
## Setup the tokenswap volume

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

## Build & Run the tokenswap image

```
NETWORK="${BLOCKFROST_NETWORK:-mainnet}" \
&& cd ~/git/nessus-cardano \
&& rm -rf ./scripts/tokenswap/context/common \
&& cp -r ./scripts/common ./scripts/tokenswap/context \
&& docker build -t nessusio/tokenswap ./scripts/tokenswap/context \
&& docker rm -f tokenswap \
&& docker run --detach  \
  --name tokenswap \
  --restart="unless-stopped" \
  -e BLOCKFROST_NETWORK="${NETWORK}" \
  -e BLOCKFROST_API_KEY="$BLOCKFROST_API_KEY" \
  -e PROXY_FROM_NAME="Percy" \
  -e PROXY_FROM_ADDR="$(cat ~/cardano/${NETWORK}/keys/acc3/payment.addr)" \
  -e PROXY_FROM_SKEY="${NETWORK}/keys/acc3/payment.skey" \
  -v tokenswap:/var/cardano/local \
  -v node-ipc:/opt/cardano/ipc \
  nessusio/tokenswap --proxy run --intrv 3600 --endless true

docker logs -f tokenswap
```
