# Alonzo White Testnet Exercise Sheet 3 "Submitting Transactions Containing Basic Plutus Scripts"

### Part 2:  Submit a transaction that uses a Plutus script.

We will first use a pre-built Plutus validator script that always returns `True`. This is the simplest possible validator script (though it is not very useful except as a placeholder/test script!).

5. Download the pre-built [AlwaysSucceeds.plutus](/resources/plutus-scripts/AlwaysSucceeds.plutus) Plutus script, and obtain the script address

```
cardano-cli address build \
  --payment-script-file /var/cardano/local/scripts/AlwaysSucceeds.plutus \
  --out-file /var/cardano/local/scratch/AlwaysSucceeds.addr \
  --testnet-magic 7

SCRIPT_ADDR=$(cat ~/cardano/scratch/AlwaysSucceeds.addr)
echo "SCRIPT_ADDR: ${SCRIPT_ADDR}"
```

6. Build a raw transaction that will submit the script and pay for it using funds from `wallet.addr`. You may need to top up the wallet before you do this if you did not transfer enough Ada initially.  For test purposes, assume that the transaction will cost 100 Ada (100,000,000 Lovelace) -- the cost of a real transaction on the Cardano Mainnet will be much less than this, of course.  Until we have improved the CLI, you will also need to specify the "budget" for executing the script in terms of time and memory execution units.  You may assume 10,000,000,000 units in each case (again, these numbers will be much larger than will be required on Mainnet).

All transaction outputs that are locked by non-native scripts must include
the hash of an additional “datum”. **A non-native script-locked output that does not include a datum hash is unspendable**

```
PAYMENT_ADDR1=$(cat ~/cardano/keys/alonzo/acc1/payment.addr)

DATUM_VALUE=29031918
cardano-cli transaction hash-script-data --script-data-value $DATUM_VALUE
DATUM_HASH="ee5c9e2778c6c398366c5b9cfd67a888081f7626ca0ac392faca5981e59ff759"

# Query Script UTOX
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 7

# Query UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 7

TX_IN1="7c7fd0f184c6a9e7053ff3c17e5b251a7748fc3c438af3c2b1c3c8d2b2cc2697#0"
TX_IN1_LVC="250000000"

# Calculate the change to send back to PAYMENT_ADDR
FEES_LVC=200000
SEND_LVC=$(($TX_IN1_LVC - $FEES_LVC))
echo "$SEND_LVC Lovelace"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --tx-in $TX_IN1 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
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

The Plutus script technically controls how the funds can be withdrawn, but it always succeeds, so there is effectively no restriction.

```
# Query Script UTOX
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 7

# Query Payment UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 7

TX_IN1="7c7fd0f184c6a9e7053ff3c17e5b251a7748fc3c438af3c2b1c3c8d2b2cc2697#1"
TX_IN1_LVC="250000000"

TX_COL="7c7fd0f184c6a9e7053ff3c17e5b251a7748fc3c438af3c2b1c3c8d2b2cc2697#2"
TX_COL_LVC="250000000"

TX_SCRIPT="53d17beade0499f9ac1f53d5dbe22df9095190f61271c9eb6546a09da0397867#0"
TX_SCRIPT_LVC="249800000"

# Calculate the change to send back to PAYMENT_ADDR
FEES_LVC=210000000
SEND_LVC=$(($TX_IN1_LVC + $TX_SCRIPT_LVC - $FEES_LVC))
echo "$SEND_LVC Lovelace"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --fee $FEES_LVC \
  --tx-in $TX_IN1 \
  --tx-in $TX_SCRIPT \
  --tx-in-script-file /var/cardano/local/scripts/AlwaysSucceeds.plutus \
  --tx-in-redeemer-value 0 \
  --tx-in-datum-value $DATUM_VALUE \
  --tx-in-execution-units "(100000000,100000000)" \
  --tx-in-collateral $TX_COL \
  --tx-out $TO_ADDR+$SEND_LVC \
  --protocol-params-file /var/cardano/local/scratch/protocol.json \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 7
```
