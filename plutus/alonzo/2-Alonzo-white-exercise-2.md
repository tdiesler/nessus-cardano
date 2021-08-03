# Alonzo White Testnet Exercise 2 (Optional): "The Hard Fork to Alonzo"

7. Additional Exercise (Moderate)

	Use some of your test ada to build and submit a simple transaction before and after the hard fork (eg a funds transfer from your original payment address to a new address that you have created - don’t try to run a Plutus script yet – we will do that in [Exercise Sheet 3](3_Alonzo-white-exercise-3.md)!).  Do you notice any differences between the two transactions (inspect the files that are built by the transaction build-raw and transaction sign commands)?  How do you check whether your transactions have succeeded?

  Now that we have gone through the hard fork into Alonzo, the next exercise will involve building, signing and submitting simple Plutus transactions using your own node.  

## Get protocol parameters

```
alias cardano-cli="docker run -it --rm \
  -v ~/cardano:/var/cardano/local \
  -v node-ipc:/opt/cardano/ipc \
  nessusio/cardano-node:${CARDANO_NODE_VERSION:-dev} cardano-cli"

cardano-cli query protocol-parameters \
  --out-file /var/cardano/local/scratch/protocol.json \
  --testnet-magic 7
```

## Query account balance

```
PAYMENT_ADDR0=$(cat ~/cardano/keys/alonzo/acc0/payment.addr)
STAKE_ADDR0=$(cat ~/cardano/keys/alonzo/acc0/stake.addr)
echo "${STAKE_ADDR0} => ${PAYMENT_ADDR0}"

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR0 \
  --testnet-magic 7

# Query rewards
cardano-cli query stake-address-info \
    --address $STAKE_ADDR0 \
    --testnet-magic 7
```

## Send some funds

```
# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR0 \
  --testnet-magic 7

TO_ADDR1=$(cat ~/cardano/keys/alonzo/acc1/payment.addr)

# Calculate the change to send back to PAYMENT_ADDR
FEES_LVC=200000
SEND_LVC1=2000000000
SEND_LVC2=2000000000
SEND_LVC3=2000000000
SEND_LVC4=2000000000
REFUND_LVC=$((1999800000 + 9902260891668 - $SEND_LVC1 - $SEND_LVC2 - $SEND_LVC3 - $SEND_LVC4 - $FEES_LVC))
echo "$REFUND_LVC Lovelace"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --tx-in "65fb041aa48fedeec58343693b9fdd6273201131a41cc21d49c20fc6e762d6b6#0" \
  --tx-in "cd1617f8203a886fcf74b8a3fba2bc4f4eefee2d79c5f3bf2d9432044a7033f6#4" \
  --tx-out $TO_ADDR1+$SEND_LVC1 \
  --tx-out $TO_ADDR1+$SEND_LVC2 \
  --tx-out $TO_ADDR1+$SEND_LVC3 \
  --tx-out $TO_ADDR1+$SEND_LVC4 \
  --tx-out $PAYMENT_ADDR0+$REFUND_LVC \
  --fee $FEES_LVC \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/acc0/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 7
```

## Send all funds

```
# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 7 | sort

TO_ADDR0=$(cat ~/cardano/keys/alonzo/acc0/payment.addr)

# Calculate the change to send back to PAYMENT_ADDR
FEES_LVC=200000
SEND_LVC=$((2*1000000000 - $FEES_LVC))
echo "$SEND_LVC Lovelace"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --tx-in "cd1617f8203a886fcf74b8a3fba2bc4f4eefee2d79c5f3bf2d9432044a7033f6#0" \
  --tx-in "cd1617f8203a886fcf74b8a3fba2bc4f4eefee2d79c5f3bf2d9432044a7033f6#1" \
  --tx-out $TO_ADDR0+$SEND_LVC \
  --fee $FEES_LVC \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 7
```

## Withdraw rewards

```
# Query rewards
cardano-cli query stake-address-info \
    --address $STAKE_ADDR \
    --testnet-magic 7

# Query payment addr
cardano-cli query utxo \
    --address $PAYMENT_ADDR \
    --testnet-magic 7

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
  --signing-key-file /var/cardano/local/keys/alonzo/${OWNER}/payment.skey \
  --signing-key-file /var/cardano/local/keys/alonzo/${OWNER}/stake.skey \
  --out-file /var/cardano/local/scratch/tx.signed

# Submit the transaction
cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 7
```

## Check the balances

```
# Query payment addr
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic 7

# Query target addr
cardano-cli query utxo \
  --address $TO_ADDR \
  --testnet-magic 7

# Query rewards
cardano-cli query stake-address-info \
  --address $STAKE_ADDR \
  --testnet-magic 7
```
