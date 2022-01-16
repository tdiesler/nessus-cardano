# ASTOR Payout

## Protocol parameters

```
alias astor="~/git/nessus-cardano/scripts/tokenswap/context/astor.tcl"

alias cardano-cli="docker run -it --rm \
  -v ~/cardano:/var/cardano/local \
  -v node-ipc:/opt/cardano/ipc \
  nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} cardano-cli"

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

cardano-cli query protocol-parameters $NETWORK_OPTION \
  --out-file /var/cardano/local/scratch/protocol.json
```
## Account Public Key Hashes

```
ACCOUNT=acc0 \
&& cardano-cli address key-hash \
  --payment-verification-key-file /var/cardano/local/$NETWORK/keys/$ACCOUNT/payment.vkey \
  --out-file /var/cardano/local/$NETWORK/keys/$ACCOUNT/payment.pkh
```

## Compile scripts

Define a Plutus _minting script_ that allows us to mint a variable number of **Astor** tokens

```
cd ~/git/nessus-cardano/plutus/tokenswap \
  && cabal run mint-tokens astormintv1.plutus \
  && mv *.plutus ~/cardano/$NETWORK/scripts \
  && POLICY_ID=$(cardano-cli transaction policyid \
    --script-file /var/cardano/local/$NETWORK/scripts/astormintv1.plutus) \
  && POLICY_ID=${POLICY_ID:0:56} \
  && echo "POLICY_ID=\"$POLICY_ID\""

POLICY_ID="891b79189cc2ad6175160655a0e6286036695af10d2511f77f966e5c"

cd ~/git/nessus-cardano/plutus/tokenswap \
  && cabal run swap-tokens astorswapv2.plutus \
  && mv *.plutus ~/cardano/$NETWORK/scripts \
  && cardano-cli address build $NETWORK_OPTION \
    --payment-script-file /var/cardano/local/$NETWORK/scripts/astorswapv2.plutus \
    --out-file /var/cardano/local/scratch/astorswapv2.addr \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/astorswapv2.addr) \
  && echo "SCRIPT_ADDR=\"${SCRIPT_ADDR}\""

ExBudget {exBudgetCPU = ExCPU 23014629, exBudgetMemory = ExMemory 77400}
SCRIPT_ADDR="addr_test1wpdke5ua4gq6y479nuzj693f6w8xjulcmt0uayzvta5p57q2zhkg9"

# Query balances
for addr in $OWNER_ADDR $SHELLEY_ADDR $MARY_ADDR $PERCY_ADDR $SCRIPT_ADDR; do
  cardano-cli query utxo $NETWORK_OPTION --address $addr
  echo
  if [[ $NETWORK == "testnet" && $addr == $OWNER_ADDR ]]; then
    cardano-cli query stake-address-info $NETWORK_OPTION \
      --address $(cat ~/cardano/testnet/keys/acc0/stake.addr)
  fi
done
```
