# Alonzo White Testnet Exercise Sheet 3 "Submitting Transactions Containing Basic Plutus Scripts"

### Part 2:  Submit a transaction that uses a Plutus script.

We will first use a pre-built Plutus validator script that always returns `True`. This is the simplest possible validator script (though it is not very useful except as a placeholder/test script!).

5. Download the pre-built [AlwaysSucceeds.plutus](/resources/plutus-scripts/AlwaysSucceeds.plutus) Plutus script, and obtain the script address

```
cd ~/git/nessus-cardano/plutus/alonzo-white/plutus-scripts \
  && cp AlwaysSucceeds.plutus ~/cardano/scripts \
  && cat ~/cardano/scripts/AlwaysSucceeds.plutus \
  && cardano-cli address build \
    --payment-script-file /var/cardano/local/scripts/AlwaysSucceeds.plutus \
    --out-file /var/cardano/local/scratch/alwayssucceeds.addr \
    --testnet-magic 7 \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/alwayssucceeds.addr) \
  && echo "SCRIPT_ADDR=\"${SCRIPT_ADDR}\""
```

6. Build a raw transaction that will submit the script and pay for it using funds from `wallet.addr`. You may need to top up the wallet before you do this if you did not transfer enough Ada initially.  For test purposes, assume that the transaction will cost 100 Ada (100,000,000 Lovelace) -- the cost of a real transaction on the Cardano Mainnet will be much less than this, of course.  Until we have improved the CLI, you will also need to specify the "budget" for executing the script in terms of time and memory execution units.  You may assume 10,000,000,000 units in each case (again, these numbers will be much larger than will be required on Mainnet).

All transaction outputs that are locked by non-native scripts must include
the hash of an additional “datum”. **A non-native script-locked output that does not include a datum hash is unspendable**

### 6.1 Lock some funds in AlwaysSucceeds

```
PAYMENT_ADDR1=$(cat ~/cardano/keys/alonzo/acc1/payment.addr)

DATUM_VALUE=29031918 \
  && DATUM_HASH="$(cardano-cli transaction hash-script-data --script-data-value $DATUM_VALUE)" \
  && DATUM_HASH=${DATUM_HASH:0:64} \
  && echo "DATUM_HASH=\"$DATUM_HASH\""

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 7

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 7

TX_IN1="790419f6ccd62ee27c9ccaa271e0351be2fa1c49b09cd69bb7672615392330b5#0"
TX_IN1_LVC="525051200"

# Calculate the change to send back to PAYMENT_ADDR
FEES_LVC=200000
SEND_LVC=100000000
REFUND_LVC=$(($TX_IN1_LVC - SEND_LVC - $FEES_LVC))
echo "$REFUND_LVC Lovelace"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --tx-in $TX_IN1 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --tx-out $PAYMENT_ADDR1+$REFUND_LVC \
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

### 6.2 Unlock the funds in AlwaysSucceeds

The Plutus script technically controls how the funds can be withdrawn, but it always succeeds, so there is effectively no restriction.

```
# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 7

# Query Payment UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 7

TX_IN1="f09af4f19ad24df42407ad151dced7aef0ca2f7c905ce709e6f35e4d44b71bc0#1"
TX_IN1_LVC="424851200"

TX_COL="9f1da07ffac8613627c52e378c9e8f4494b23e17df4709b93d47d0021617e153#0"
TX_COL_LVC="1100000000"

TX_SCRIPT="f09af4f19ad24df42407ad151dced7aef0ca2f7c905ce709e6f35e4d44b71bc0#0"
TX_SCRIPT_LVC="100000000"

# Calculate the change to send back to PAYMENT_ADDR
ExCPU=4081000
ExMem=790
ExFct=20
UNITS="($(($ExFct*$ExCPU)),$(($ExFct*$ExMem)))"
FEES_LVC=$(($ExFct * ($ExCPU + $ExMem) + 500000))
SEND_LVC=$(($TX_IN1_LVC + $TX_SCRIPT_LVC - $FEES_LVC))
echo "Send=$SEND_LVC, Fees=$FEES_LVC, Units=$UNITS"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --fee $FEES_LVC \
  --tx-in $TX_IN1 \
  --tx-in $TX_SCRIPT \
  --tx-in-script-file /var/cardano/local/scripts/AlwaysSucceeds.plutus \
  --tx-in-redeemer-value 0 \
  --tx-in-datum-value $DATUM_VALUE \
  --tx-in-execution-units "$UNITS" \
  --tx-in-collateral $TX_COL \
  --tx-out $PAYMENT_ADDR1+$SEND_LVC \
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

Failure to provide the correct --tx-in-datum-value results in MissingRequiredDatums

### 6.3 Lock some funds in AlwaysFails

```
cd ~/git/nessus-cardano/plutus/alonzo-white/plutus-sources/plutus-alwaysfails \
  && cabal run plutus-alwaysfails 1 AlwaysFails.plutus \
  && cp AlwaysFails.plutus ../../plutus-scripts \
  && mv AlwaysFails.plutus ~/cardano/scripts \
  && cardano-cli address build \
    --payment-script-file /var/cardano/local/scripts/AlwaysFails.plutus \
    --out-file /var/cardano/local/scratch/alwaysfails.addr \
    --testnet-magic 7 \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/alwaysfails.addr) \
  && echo "SCRIPT_ADDR=\"${SCRIPT_ADDR}\""

ExBudget {_exBudgetCPU = ExCPU 4198000, _exBudgetMemory = ExMemory 820}
SCRIPT_ADDR="addr_test1wrjdxkv9acxf7hftv6qpc7pz6r7sr9raqkupxh9uhsk0j8gfvhphg"
```

### 6.4 Lock some funds in AlwaysFails

```
PAYMENT_ADDR1=$(cat ~/cardano/keys/alonzo/acc1/payment.addr)

DATUM_VALUE=29031918 \
  && DATUM_HASH="$(cardano-cli transaction hash-script-data --script-data-value $DATUM_VALUE)" \
  && DATUM_HASH=${DATUM_HASH:0:64} \
  && echo "DATUM_HASH=\"$DATUM_HASH\""

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 7

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 7

TX_IN1="24c34c42d33622e2e08521310d7becae97e6ddd54567a7ceeedf670c929440de#1"
TX_IN1_LVC="333690200"

# Calculate the change to send back to PAYMENT_ADDR
FEES_LVC=200000
SEND_LVC=100000000
REFUND_LVC=$(($TX_IN1_LVC - SEND_LVC - $FEES_LVC))
echo "$REFUND_LVC Lovelace"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --tx-in $TX_IN1 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --tx-out $PAYMENT_ADDR1+$REFUND_LVC \
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

### 6.5 Unlock the funds in alwaysfails

The Plutus script technically controls how the funds can be withdrawn, but it always fails.

```
# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 7

# Query Payment UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 7

TX_IN1="29f82f40603c4328e6efffb7c6e8851fe9540d18ddc0930b188896f6b016e141#1"
TX_IN1_LVC="233490200"

TX_COL="9f1da07ffac8613627c52e378c9e8f4494b23e17df4709b93d47d0021617e153#0"
TX_COL_LVC="1100000000"

TX_SCRIPT="29f82f40603c4328e6efffb7c6e8851fe9540d18ddc0930b188896f6b016e141#0"
TX_SCRIPT_LVC="100000000"

# Calculate the change to send back to PAYMENT_ADDR
ExCPU=4081000
ExMem=790
ExFct=20
UNITS="($(($ExFct*$ExCPU)),$(($ExFct*$ExMem)))"
FEES_LVC=$(($ExFct * ($ExCPU + $ExMem) + 500000))
SEND_LVC=$(($TX_IN1_LVC + $TX_SCRIPT_LVC - $FEES_LVC))
echo "Send=$SEND_LVC, Fees=$FEES_LVC, Units=$UNITS"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --fee $FEES_LVC \
  --tx-in $TX_IN1 \
  --tx-in $TX_SCRIPT \
  --tx-in-script-file /var/cardano/local/scripts/AlwaysFails.plutus \
  --tx-in-redeemer-value 0 \
  --tx-in-datum-value "$DATUM_VALUE" \
  --tx-in-execution-units "$UNITS" \
  --tx-in-collateral $TX_COL \
  --tx-out $PAYMENT_ADDR1+$SEND_LVC \
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

Transaction can successfully submitted, the validator fails silently and the collateral is removed
