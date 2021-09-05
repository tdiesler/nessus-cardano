# Alonzo Purple Testnet Exercise 2 (Optional): "The Hard Fork to Alonzo"

7. Additional Exercise (Moderate)

	Use some of your test ada to build and submit a simple transaction before and after the hard fork (eg a funds transfer from your original payment address to a new address that you have created - don’t try to run a Plutus script yet – we will do that in [Exercise Sheet 3](3_Alonzo-purple-exercise-3.md)!).  Do you notice any differences between the two transactions (inspect the files that are built by the transaction build-raw and transaction sign commands)?  How do you check whether your transactions have succeeded?

  Now that we have gone through the hard fork into Alonzo, the next exercise will involve building, signing and submitting simple Plutus transactions using your own node.  

## Run the node

```
docker run --detach \
    --name=testrl\
    -p 3001:3001 \
    -e CARDANO_NETWORK=testnet \
    -v test-data:/opt/cardano/data \
    -v node-ipc:/opt/cardano/ipc \
    nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} run

docker logs -f testrl

docker exec -it testrl gLiveView
```

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

## Query account balance

```
PAYMENT_ADDR0=$(cat ~/cardano/keys/testnet/acc0/payment.addr) \
&& PAYMENT_ADDR1=$(cat ~/cardano/keys/testnet/acc1/payment.addr) \
&& PAYMENT_ADDR2=$(cat ~/cardano/keys/testnet/acc2/payment.addr) \
&& PAYMENT_ADDR3=$(cat ~/cardano/keys/testnet/acc3/payment.addr) \
&& echo "acc0: ${PAYMENT_ADDR0}" \
&& echo "acc1: ${PAYMENT_ADDR1}" \
&& echo "acc2: ${PAYMENT_ADDR2}" \
&& echo "acc3: ${PAYMENT_ADDR3}"

# Query balances
for i in 0 1 2 3; do
  USER="acc$i"
  echo "[$USER] ==============================================================================="
  cardano-cli query utxo \
    --address $(cat ~/cardano/keys/testnet/$USER/payment.addr) \
    --testnet-magic $TESTNET_MAGIC
  echo
done

# Query rewards
cardano-cli query stake-address-info \
  --address $STAKE_ADDR0 \
  --testnet-magic $TESTNET_MAGIC
```

## Send some funds

```
FROM_ADDR=$PAYMENT_ADDR0
TO_ADDR1=$PAYMENT_ADDR1
TO_ADDR2=$PAYMENT_ADDR2
TO_ADDR3=$PAYMENT_ADDR3

SEND_LVC=200000000

# Query UTxO
cardano-cli query utxo \
  --address $FROM_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="076f062a8d038709ff78943339b03196636366c45439b89135949163a9138232#0"

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN1 \
  --tx-out $TO_ADDR1+$SEND_LVC \
  --tx-out $TO_ADDR1+$SEND_LVC \
  --tx-out $TO_ADDR2+$SEND_LVC \
  --tx-out $TO_ADDR2+$SEND_LVC \
  --tx-out $TO_ADDR3+$SEND_LVC \
  --tx-out $TO_ADDR3+$SEND_LVC \
  --change-address $FROM_ADDR \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc0/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

## Send all funds

```
FROM_ADDR=$PAYMENT_ADDR0
TO_ADDR=$PAYMENT_ADDR0

# Query UTxO
cardano-cli query utxo \
  --address $FROM_ADDR \
  --testnet-magic $TESTNET_MAGIC | sort

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in "47638df05db2ca4ef9613977a27f8a606130e631c63c9c2f5ccf6c6776332d82#0" \
  --tx-in "c06233327e078487f5338a706e7c29cdcfef7b18e4d40ac15fded0388bef94c2#0" \
  --change-address $TO_ADDR \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc0/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

## Withdraw rewards

```
# Query rewards
cardano-cli query stake-address-info \
    --address $STAKE_ADDR \
    --testnet-magic $TESTNET_MAGIC

# Query payment addr
cardano-cli query utxo \
    --address $PAYMENT_ADDR \
    --testnet-magic $TESTNET_MAGIC

TX_IN1="7962075bc605010fcab7aec2d4aafa8f5c7460cc866ceb87e82359eb8594e0d6#1"
UTXO_LVC=9998096244191
REWARD_LVC=27892557277

FEES_LVC=200000
SEND_LVC=`expr $UTXO_LVC + $REWARD_LVC - $FEES_LVC`
echo "$SEND_LVC Lovelace"

# Build the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --tx-in ${TX_IN1} \
  --tx-out ${PAYMENT_ADDR}+${SEND_LVC} \
  --withdrawal ${STAKE_ADDR}+${REWARD_LVC} \
  --fee $FEES_LVC \
  --out-file /var/cardano/local/scratch/tx.raw

# Sign the transaction
cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/${OWNER}/payment.skey \
  --signing-key-file /var/cardano/local/keys/testnet/${OWNER}/stake.skey \
  --out-file /var/cardano/local/scratch/tx.signed

# Submit the transaction
cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```
