# Alonzo White Testnet Exercise 2 (Optional): "The Hard Fork to Alonzo"

7. Additional Exercise (Moderate)

	Use some of your test ada to build and submit a simple transaction before and after the hard fork (eg a funds transfer from your original payment address to a new address that you have created - don’t try to run a Plutus script yet – we will do that in [Exercise Sheet 3](3_Alonzo-white-exercise-3.md)!).  Do you notice any differences between the two transactions (inspect the files that are built by the transaction build-raw and transaction sign commands)?  How do you check whether your transactions have succeeded?

  Now that we have gone through the hard fork into Alonzo, the next exercise will involve building, signing and submitting simple Plutus transactions using your own node.  

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

## Query account balance

```
PAYMENT_ADDR0=$(cat ~/cardano/keys/alonzo/acc0/payment.addr)
STAKE_ADDR0=$(cat ~/cardano/keys/alonzo/acc0/stake.addr)
echo "${STAKE_ADDR0} => ${PAYMENT_ADDR0}"

# Query UTOX
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
# Query UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR0 \
  --testnet-magic 7

TX_IN1="2fb82756c9816a41bff75bbb72791015c1c2508a8971868075925204db5d9492#0"
TX_IN1_LVC="10026329201468"

TO_ADDR=$(cat ~/cardano/keys/alonzo/acc1/payment.addr)
REFUND_ADDR="$PAYMENT_ADDR0"

# Calculate the change to send back to PAYMENT_ADDR
FEES_LVC=200000
SEND_LVC1=250000000
SEND_LVC2=250000000
SEND_LVC3=250000000
SEND_LVC4=250000000
REFUND_LVC=$(($TX_IN1_LVC - $SEND_LVC1 - $SEND_LVC2 - $SEND_LVC3 - $SEND_LVC4 - $FEES_LVC))
echo "$REFUND_LVC Lovelace"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --tx-in $TX_IN1 \
  --tx-out $TO_ADDR+$SEND_LVC1 \
  --tx-out $TO_ADDR+$SEND_LVC2 \
  --tx-out $TO_ADDR+$SEND_LVC3 \
  --tx-out $TO_ADDR+$SEND_LVC4 \
  --tx-out $REFUND_ADDR+$REFUND_LVC \
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
# Query UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 7 | sort

TX_IN1="167e0eb0259f84e6995c679f05ef0ad2978c28244766e3fdfb275b7a4be2d5f2#0"
TX_IN1_LVC="10000000"

TO_ADDR=$(cat ~/cardano/keys/alonzo/acc0/payment.addr)

# Calculate the change to send back to PAYMENT_ADDR
FEES_LVC=200000
SEND_LVC=$(($TX_IN1_LVC - $FEES_LVC))
echo "$SEND_LVC Lovelace"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --tx-in $TX_IN1 \
  --tx-out $TO_ADDR+$SEND_LVC \
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
UTOX_LVC=9998096244191
REWARD_LVC=27892557277

FEES_LVC=200000
SEND_LVC=`expr $UTOX_LVC + $REWARD_LVC - $FEES_LVC`
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