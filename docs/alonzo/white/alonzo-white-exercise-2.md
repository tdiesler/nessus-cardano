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
PAYMENT_ADDR=$(cat ~/cardano/keys/alonzo/acc0/payment.addr)
STAKE_ADDR=$(cat cardano/keys/alonzo/acc0/stake.addr)
echo "${STAKE_ADDR} => ${PAYMENT_ADDR}"

# Query UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic 7

# Query rewards
cardano-cli query stake-address-info \
    --address ${STAKE_ADDR} \
    --testnet-magic 7
```

# Send some coin

```
# Query UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic 7

  TxHash                                                          TxIx        Amount
--------------------------------------------------------------------------------------
efafdac2325cca3153f7c2dc2f0c326afea47cf6c9c53f2e5deeb878e1ff058a     1        9999397232308 lovelace + TxOutDatumHashNone

TX_IN1="efafdac2325cca3153f7c2dc2f0c326afea47cf6c9c53f2e5deeb878e1ff058a#1"

UTOX_LVC1="9999397232308"

TO_ADDR=$(cat ~/cardano/keys/alonzo/acc1/payment.addr)
REFUND_ADDR="$PAYMENT_ADDR"

cardano-cli transaction build-raw \
  --tx-in $TX_IN1 \
  --tx-out $TO_ADDR+0 \
  --tx-out $REFUND_ADDR+0 \
  --ttl 0 \
  --fee 0 \
  --out-file /var/cardano/local/scratch/tx.draft

# Calculate the fee  

cardano-cli transaction calculate-min-fee \
  --protocol-params-file /var/cardano/local/scratch/protocol.json \
  --tx-body-file /var/cardano/local/scratch/tx.draft \
  --tx-in-count 1 \
  --tx-out-count 2 \
  --witness-count 1 \
  --testnet-magic 7

>  206641 Lovelace

# Calculate the change to send back to PAYMENT_ADDR

SEND_LVC=100000000
FEES_LVC=206641
REFUND_LVC=$(($UTOX_LVC1 - $SEND_LVC - $FEES_LVC))
echo "$REFUND_LVC Lovelace"

# Determine the TTL (time to Live) for the transaction

SLOTNO=$(cardano-cli query tip --testnet-magic 7 | jq -c | jq ".slot")

TTL=$(($SLOTNO + 3600))
echo -e "SlotNo: $SLOTNO\nTTL:    $TTL"

# Build the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --tx-in $TX_IN1 \
  --tx-out $TO_ADDR+$SEND_LVC \
  --tx-out $REFUND_ADDR+$REFUND_LVC \
  --ttl $TTL \
  --fee $FEES_LVC \
  --out-file /var/cardano/local/scratch/tx.raw

# Sign the transaction
cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/acc0/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed

# Submit the transaction
cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
```

## Check the balances

```
cardano-cli query utxo \
    --address $TO_ADDR \
    --testnet-magic 7

cardano-cli query utxo \
    --address $PAYMENT_ADDR \
    --testnet-magic 7
```
