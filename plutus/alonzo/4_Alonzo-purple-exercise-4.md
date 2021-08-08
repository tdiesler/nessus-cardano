## Alonzo Purple Testnet Exercise Sheet 4: "Compiling and Submitting Simple Plutus Transactions"


In the third exercise, you submitted a pre-compiled transaction to the Alonzo testnet using the node CLI, and made sure you could use the test ada that had been given to you. In this exercise, you will write some very simple Plutus transactions, calculate the fees for those transactions, and submit them to the testnet.

## Prerequisites ##
1. Complete [Exercise 3](3_Alonzo-purple-exercise-3.md)
2. Start a passive Cardano node if you need to, and make sure that it has synced with the testnet. It should be in the Alonzo era.
3. Make sure you have some Alonzo Purple test ada
4. Check out the [resources](../../resources) directory for useful sources and scripts etc.

## Objectives ##

In the fourth set of exercises, we will make sure that you can:

1. Compile and submit simple Plutus transactions
2. Calculate fees for Plutus transactions
3. Determine what effect your Plutus transactions have had on the blockchain

## Exercises ##

1. Compile the `AlwaysSucceeds` Plutus script from [source](../../resources/plutus-sources/plutus-alwayssucceeds) and extract the serialised representation that the compiler has produced.

### 1.1 Compile AlwaysSucceeds

```
cd ~/git/nessus-cardano/plutus/alonzo-purple/plutus-sources/plutus-alwayssucceeds \
  && cabal run plutus-alwayssucceeds 1 alwayssucceeds.plutus \
  && cp alwayssucceeds.plutus ../../plutus-scripts \
  && mv alwayssucceeds.plutus ~/cardano/scripts \
  && cat ~/cardano/scripts/alwayssucceeds.plutus \
  && cardano-cli address build \
    --payment-script-file /var/cardano/local/scripts/alwayssucceeds.plutus \
    --out-file /var/cardano/local/scratch/alwayssucceeds.addr \
    --testnet-magic 8 \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/alwayssucceeds.addr) \
  && echo "SCRIPT_ADDR=${SCRIPT_ADDR}"

ExBudget {_exBudgetCPU = ExCPU 1390000, _exBudgetMemory = ExMemory 100}
{
    "type": "PlutusScriptV1",
    "description": "",
    "cborHex": "4e4d01000033222220051200120011"
}
SCRIPT_ADDR=addr_test1wpnlxv2xv9a9ucvnvzqakwepzl9ltx7jzgm53av2e9ncv4sysemm8
```

You may also want to inspect the Plutus Core form – this is a representation of the compiled script that will be executed on the blockchain. Confirm that your script is identical to the pre-compiled version that you used in [Exercise 3](3_Alonzo-purple-exercise-3.md). If not, how is it different, and why?

2. Compile the `HelloWorld` Plutus script from [source](../../resources/plutus-sources/plutus-helloworld). Save the serialised Plutus script into a file `helloworld.plutus`.

### 2.1 Compile HelloWorld (Static Value)

```
cd ~/git/nessus-cardano/plutus/alonzo-purple/plutus-sources/plutus-helloworld \
  && cabal run plutus-helloworld 1 helloworld.plutus \
  && cp helloworld.plutus ../../plutus-scripts \
  && mv helloworld.plutus ~/cardano/scripts \
  && cardano-cli address build \
    --payment-script-file /var/cardano/local/scripts/helloworld.plutus \
    --out-file /var/cardano/local/scratch/helloworld.addr \
    --testnet-magic 8 \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/helloworld.addr) \
  && echo "SCRIPT_ADDR=\"${SCRIPT_ADDR}\""

ExBudget {_exBudgetCPU = ExCPU 2443000, _exBudgetMemory = ExMemory 370}
SCRIPT_ADDR="addr_test1wz76075duuumrem07h6s7sskrjnvra4n7zepekuju3fl5dskq3na6"
```

3. Build a Cardano transaction that will submit `helloworld.plutus` for execution on the testnet.  As before, you will need to provide two input addresses: one to pay for the transaction fees and one to provide the collateral that is spent if the Plutus script fails to validate (to avoid risking all your funds, it is recommended that you use a dedicated payment address with limited funds to pay for the collateral rather than your main payment address!).

### 3.1 Lock some funds in HelloWorld (Static Value)

We create a UTxO locked by the script that holds an integer datum hash.

```
PAYMENT_ADDR1=$(cat ~/cardano/keys/alonzo/acc1/payment.addr)

DATUM_HEX=$(printf "0x%s" $(echo -n 'Hello!' | xxd -ps)) \
  && echo -e "$DATUM_HEX" \
  && DATUM_VALUE=$(printf "%d" $DATUM_HEX) \
  && echo $DATUM_VALUE \
  && DATUM_HASH="$(cardano-cli transaction hash-script-data --script-data-value $DATUM_VALUE)" \
  && DATUM_HASH=${DATUM_HASH:0:64} \
  && echo "DATUM_HASH=\"$DATUM_HASH\""

# [BUG] - 'cardano-cli transaction hash-script-data' appends whitespace
# https://github.com/input-output-hk/cardano-node/issues/2937

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 8

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 8

TX_IN1="902501acfd1706d60f476949ab4aee405bab422511f6b4ad0b1a3c9ec384c04a#0"
TX_IN1_LVC="1942286100"

# Calculate the change to send back to PAYMENT_ADDR
FEES_LVC=200000
SEND_LVC=100000000
REFUND_LVC=$(($TX_IN1_LVC - SEND_LVC - $FEES_LVC))
echo "$REFUND_LVC Lovelace"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --fee $FEES_LVC \
  --tx-in $TX_IN1 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --tx-out $PAYMENT_ADDR1+$REFUND_LVC \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

### 3.2 Unlock the funds in HelloWorld (Static Value)

```
# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 8

# Query Payment UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 8

TX_IN1="07ec52f8afba48341bf956ffa2c053fe29ebf161f2767b165f11a615ec4eb196#1"
TX_IN1_LVC="1842086100"

TX_COL="1479fbe7e80e672e6c75897185dca0ff6771e17d91cd7da175694e4b5b1c24e0#3"
TX_COL_LVC="2000000000"

TX_SCRIPT="07ec52f8afba48341bf956ffa2c053fe29ebf161f2767b165f11a615ec4eb196#0"
TX_SCRIPT_LVC="100000000"

# Calculate the change to send back to PAYMENT_ADDR
ExCPU=2443000
ExMem=370
ExFct=3
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
  --tx-in-script-file /var/cardano/local/scripts/helloworld.plutus \
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
  --testnet-magic 8
```

4. Modify the `HelloWorld` Plutus script:

a. To succeed if  the datum is your name

### 4a1 Build the validator script

```
cd ~/git/nessus-cardano/plutus/alonzo-purple/plutus-sources/plutus-helloworld \
  && cabal run plutus-helloworld4a 1 helloworld4a.plutus \
  && cp helloworld4a.plutus ../../plutus-scripts \
  && mv helloworld4a.plutus ~/cardano/scripts \
  && cardano-cli address build \
    --payment-script-file /var/cardano/local/scripts/helloworld4a.plutus \
    --out-file /var/cardano/local/scratch/helloworld4a.addr \
    --testnet-magic 8 \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/helloworld4a.addr) \
  && echo "SCRIPT_ADDR=\"${SCRIPT_ADDR}\""

ExBudget {_exBudgetCPU = ExCPU 24166000, _exBudgetMemory = ExMemory 5940}
"Datum value: {\"bytes\":\"54686f6d6173\"}"
SCRIPT_ADDR="addr_test1wpe7p9ya9rx2qz09mjmj736pg9ztsdjj68q5c8n4e4q3q8qe4k548"
```

### 4a2 Lock some funds in the script

```
PAYMENT_ADDR1=$(cat ~/cardano/keys/alonzo/acc1/payment.addr)

DATUM_VALUE='Thomas' \
  && DATUM_JSON=$(printf "{\"bytes\":\"%s\"}" $(echo -n $DATUM_VALUE | xxd -ps)) \
  && echo "$DATUM_JSON" > ~/cardano/scratch/script-datum.json \
  && echo $DATUM_JSON \
  && DATUM_HASH="$(cardano-cli transaction hash-script-data --script-data-file /var/cardano/local/scratch/script-datum.json)" \
  && DATUM_HASH=${DATUM_HASH:0:64} \
  && echo "DATUM_HASH=\"$DATUM_HASH\""

# [BUG] - 'cardano-cli transaction hash-script-data' appends whitespace
# https://github.com/input-output-hk/cardano-node/issues/2937

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 8

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 8

TX_IN1="5f8cf0944f0b46909de35e316362c3067cbc51b4b34eba8b6b8643e0a9e374d0#0"
TX_IN1_LVC="1934255990"

# Calculate the change to send back to PAYMENT_ADDR
FEES_LVC=200000
SEND_LVC=10000000
REFUND_LVC=$(($TX_IN1_LVC - SEND_LVC - $FEES_LVC))
echo "$REFUND_LVC Lovelace"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --fee $FEES_LVC \
  --tx-in $TX_IN1 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --tx-out $PAYMENT_ADDR1+$REFUND_LVC \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

### 4a3 Unlock the funds in the the script

```
# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 8

# Query Payment UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 8

TX_IN1="be0ab2d6e22143943988af33685c6152ff7f14a191ead4a0283edc02722c4f67#1"
TX_IN1_LVC="1924055990"

TX_COL="1479fbe7e80e672e6c75897185dca0ff6771e17d91cd7da175694e4b5b1c24e0#2"
TX_COL_LVC="2000000000"

TX_SCRIPT="be0ab2d6e22143943988af33685c6152ff7f14a191ead4a0283edc02722c4f67#0"
TX_SCRIPT_LVC="10000000"

# Calculate the change to send back to PAYMENT_ADDR
ExCPU=24166000
ExMem=5940
ExFct=30
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
  --tx-in-script-file /var/cardano/local/scripts/helloworld4a.plutus \
  --tx-in-datum-file /var/cardano/local/scratch/script-datum.json \
  --tx-in-redeemer-value '""' \
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
  --testnet-magic 8
```

### 4b1 Build the validator script

b. To succeed if the redeemer is also your birthday;

```
cd ~/git/nessus-cardano/plutus/alonzo-purple/plutus-sources/plutus-helloworld \
  && cabal run plutus-helloworld4b 1 helloworld4b.plutus \
  && cp helloworld4b.plutus ../../plutus-scripts \
  && mv helloworld4b.plutus ~/cardano/scripts \
  && cardano-cli address build \
    --payment-script-file /var/cardano/local/scripts/helloworld4b.plutus \
    --out-file /var/cardano/local/scratch/helloworld4b.addr \
    --testnet-magic 8 \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/helloworld4b.addr) \
  && echo "SCRIPT_ADDR=\"${SCRIPT_ADDR}\""

ExBudget {_exBudgetCPU = ExCPU 24634000, _exBudgetMemory = ExMemory 6060}
"Datum value: {\"bytes\":\"4865696e7a\"}"
SCRIPT_ADDR="addr_test1wq3cj6cu23wy5eh9z5jxk7cekq4nnnz6nldw7udsnpamrvgckts58"
```

### 4b2 Lock some funds in the script

```
PAYMENT_ADDR1=$(cat ~/cardano/keys/alonzo/acc1/payment.addr)

DATUM_VALUE='Heinz' \
  && DATUM_JSON=$(printf "{\"bytes\":\"%s\"}" $(echo -n $DATUM_VALUE | xxd -ps)) \
  && echo "$DATUM_JSON" > ~/cardano/scratch/script-datum.json \
  && echo $DATUM_JSON \
  && DATUM_HASH="$(cardano-cli transaction hash-script-data --script-data-file /var/cardano/local/scratch/script-datum.json)" \
  && DATUM_HASH=${DATUM_HASH:0:64} \
  && echo "DATUM_HASH=\"$DATUM_HASH\""

# [BUG] - 'cardano-cli transaction hash-script-data' appends whitespace
# https://github.com/input-output-hk/cardano-node/issues/2937

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 8

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 8

TX_IN1="bf7034853984be0495c0fca4212945893bd25649ea04363fab1ac69a1be84d6b#0"
TX_IN1_LVC="1208397790"

# Calculate the change to send back to PAYMENT_ADDR
FEES_LVC=200000
SEND_LVC=10000000
REFUND_LVC=$(($TX_IN1_LVC - SEND_LVC - $FEES_LVC))
echo "$REFUND_LVC Lovelace"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --fee $FEES_LVC \
  --tx-in $TX_IN1 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --tx-out $PAYMENT_ADDR1+$REFUND_LVC \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

### 4b3 Unlock the funds in the the script

```
# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 8

# Query Payment UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 8

TX_IN1="4cc71a6afb4c6afb362d8ce25f32fb025f9fac54afee81e635e9f3216c74f1e4#1"
TX_IN1_LVC="1198197790"

TX_COL="1479fbe7e80e672e6c75897185dca0ff6771e17d91cd7da175694e4b5b1c24e0#3"
TX_COL_LVC="2000000000"

TX_SCRIPT="4cc71a6afb4c6afb362d8ce25f32fb025f9fac54afee81e635e9f3216c74f1e4#0"
TX_SCRIPT_LVC="10000000"

# Calculate the change to send back to PAYMENT_ADDR
ExCPU=24634000
ExMem=6060
ExFct=30
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
  --tx-in-script-file /var/cardano/local/scripts/helloworld4b.plutus \
  --tx-in-datum-file /var/cardano/local/scratch/script-datum.json \
  --tx-in-redeemer-value 29031918 \
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
  --testnet-magic 8
```

c. To take a datum and a redeemer and to succeed if the redeemer is the same as the datum;

### 4d1 Build the validator script

d. To take a datum that represents a pair of integers and a redeemer that represents a list of integers and succeed if all elements in the redeemer are within the range specified by the values in the datum.

```
cd ~/git/nessus-cardano/plutus/alonzo-purple/plutus-sources/plutus-helloworld \
  && cabal run plutus-helloworld4d 1 helloworld4d.plutus \
  && cp helloworld4d.plutus ../../plutus-scripts \
  && mv helloworld4d.plutus ~/cardano/scripts \
  && cardano-cli address build \
    --payment-script-file /var/cardano/local/scripts/helloworld4d.plutus \
    --out-file /var/cardano/local/scratch/helloworld4d.addr \
    --testnet-magic 8 \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/helloworld4d.addr) \
  && echo "SCRIPT_ADDR=\"${SCRIPT_ADDR}\""

ExBudget {_exBudgetCPU = ExCPU 29509000, _exBudgetMemory = ExMemory 7310}
SCRIPT_ADDR="addr_test1wrj7qzf0vfzszyk45pulsszynkgp5zp60cv4sz2tts7wkjq04pyy8"
```

### 4d2 Lock some funds in the script

```
PAYMENT_ADDR1=$(cat ~/cardano/keys/alonzo/acc1/payment.addr)

DATUM_VALUE='[1,5]' \
  && DATUM_HASH="$(cardano-cli transaction hash-script-data --script-data-value $DATUM_VALUE)" \
  && DATUM_HASH=${DATUM_HASH:0:64} \
  && echo "DATUM_HASH=\"$DATUM_HASH\""

# [BUG] - 'cardano-cli transaction hash-script-data' appends whitespace
# https://github.com/input-output-hk/cardano-node/issues/2937

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 8

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 8

TX_IN1="35616f8989a16128c86278d7f6d20d07a5c7ba9fc51b20362e4518cedeb1292e#0"
TX_IN1_LVC="468495990"

# Calculate the change to send back to PAYMENT_ADDR
FEES_LVC=200000
SEND_LVC=10000000
REFUND_LVC=$(($TX_IN1_LVC - SEND_LVC - $FEES_LVC))
echo "$REFUND_LVC Lovelace"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --fee $FEES_LVC \
  --tx-in $TX_IN1 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --tx-out $PAYMENT_ADDR1+$REFUND_LVC \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

### 4d3 Unlock the funds in the the script

```
# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 8

# Query Payment UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 8

TX_IN1="2710f90eb095f20583b0985694ca057db614dc3fa455637c60ab0eeb34e6b1b2#1"
TX_IN1_LVC="458295990"

TX_COL="1479fbe7e80e672e6c75897185dca0ff6771e17d91cd7da175694e4b5b1c24e0#3"
TX_COL_LVC="2000000000"

TX_SCRIPT="2710f90eb095f20583b0985694ca057db614dc3fa455637c60ab0eeb34e6b1b2#0"
TX_SCRIPT_LVC="10000000"

# Calculate the change to send back to PAYMENT_ADDR
ExCPU=29509000
ExMem=7310
ExFct=30
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
  --tx-in-script-file /var/cardano/local/scripts/helloworld4d.plutus \
  --tx-in-datum-value '[1,5]' \
  --tx-in-redeemer-value '[1,2,3,4]' \
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
  --testnet-magic 8
```

## 5 Set up new payment addresses for wallet1 & wallet2

Set up three new payment addresses: `payment.addr`, `wallet1.addr`, and `wallet2.addr` using the node CLI commands.  

```
mkdir -p ~/cardano/keys/alonzo/wallets

for x in 1 2; do
  WALLET="wallet$x"
  cardano-cli address key-gen \
    --verification-key-file /var/cardano/local/keys/alonzo/wallets/${WALLET}.vkey \
    --signing-key-file /var/cardano/local/keys/alonzo/wallets/${WALLET}.skey \
  && cardano-cli address build \
    --payment-verification-key-file /var/cardano/local/keys/alonzo/wallets/${WALLET}.vkey \
    --out-file /var/cardano/local/keys/alonzo/wallets/${WALLET}.addr \
    --testnet-magic 8 \
  && cardano-cli address key-hash \
    --payment-verification-key-file /var/cardano/local/keys/alonzo/wallets/${WALLET}.vkey \
    --out-file /var/cardano/local/scratch/${WALLET}-pubkey.hash
done

# Extract the PubKey Hashes
cardano-cli address key-hash \
  --payment-verification-key-file /var/cardano/local/keys/alonzo/wallets/wallet1.vkey \
  --out-file /var/cardano/local/scratch/wallet1-pubkey.hash \
&& cardano-cli address key-hash \
  --payment-verification-key-file /var/cardano/local/keys/alonzo/wallets/wallet2.vkey \
  --out-file /var/cardano/local/scratch/wallet2-pubkey.hash \
&& WALLET_HASH1="$(cat ~/cardano/scratch/wallet1-pubkey.hash)" \
&& WALLET_HASH2="$(cat ~/cardano/scratch/wallet2-pubkey.hash)" \
&& echo "WALLET_HASH1=$WALLET_HASH1" \
&& echo "WALLET_HASH2=$WALLET_HASH2"
```

Transfer some ada to each of these addresses, and check that they have been funded.

```
PAYMENT_ADDR0=$(cat ~/cardano/keys/alonzo/acc0/payment.addr)
WALLET_ADDR1=$(cat ~/cardano/keys/alonzo/wallets/wallet1.addr)
WALLET_ADDR2=$(cat ~/cardano/keys/alonzo/wallets/wallet2.addr)

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR0 \
  --testnet-magic 8

TX_IN1="09a0e6c42682440dbd71cf1db0ddb5b35701d2dc8e8f2aee837db64e71b0ab85#2"
TX_IN1_LVC="987497200000"

# Calculate the change to send back to PAYMENT_ADDR
FEES_LVC=200000
SEND_LVC=2000000000
REFUND_LVC=$(($TX_IN1_LVC - 6*$SEND_LVC - $FEES_LVC))
echo "$REFUND_LVC Lovelace"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --tx-in $TX_IN1 \
  --tx-out $WALLET_ADDR1+$SEND_LVC \
  --tx-out $WALLET_ADDR1+$SEND_LVC \
  --tx-out $WALLET_ADDR1+$SEND_LVC \
  --tx-out $WALLET_ADDR2+$SEND_LVC \
  --tx-out $WALLET_ADDR2+$SEND_LVC \
  --tx-out $WALLET_ADDR2+$SEND_LVC \
  --tx-out $PAYMENT_ADDR0+$REFUND_LVC \
  --fee $FEES_LVC \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/acc0/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8

# Query UTxO
cardano-cli query utxo \
  --address $WALLET_ADDR1 \
  --testnet-magic 8 \
&& cardano-cli query utxo \
  --address $WALLET_ADDR2 \
  --testnet-magic 8
```

Produce a transaction that sends 100 ada from `wallet1.addr` to `wallet2.addr` provided the correct “secret spending key” is provided as a redeemer and submit this.

```
cd ~/git/nessus-cardano/plutus/alonzo-purple/plutus-sources/plutus-helloworld \
  && cabal run plutus-helloworld5a 1 helloworld5a.plutus \
  && cp helloworld5a.plutus ../../plutus-scripts \
  && mv helloworld5a.plutus ~/cardano/scripts \
  && cardano-cli address build \
    --payment-script-file /var/cardano/local/scripts/helloworld5a.plutus \
    --out-file /var/cardano/local/scratch/helloworld5a.addr \
    --testnet-magic 8 \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/helloworld5a.addr) \
  && echo "SCRIPT_ADDR=${SCRIPT_ADDR}"

ExBudget {_exBudgetCPU = ExCPU 29392000, _exBudgetMemory = ExMemory 7280}
SCRIPT_ADDR=addr_test1wp5aunhx9x7k0v86agmsfpcsm9w9vj6u9gwde4gxzk8a3gq95cqrw
```

### 5a1 Lock some funds in the script

```
PAYMENT_ADDR1=$(cat ~/cardano/keys/alonzo/acc1/payment.addr)

DATUM_HASH="$(cardano-cli transaction hash-script-data --script-data-value 0)" \
  && DATUM_HASH=${DATUM_HASH:0:64} \
  && echo "DATUM_HASH=\"$DATUM_HASH\""

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 8

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 8

TX_IN1="af41245a1e28831452fbe7cf43b39086cdf383e749e4abf724c306f6f8a8bb2c#0"
TX_IN1_LVC="1582306690"

# Calculate the change to send back to PAYMENT_ADDR
FEES_LVC=200000
SEND_LVC=10000000
REFUND_LVC=$(($TX_IN1_LVC - SEND_LVC - $FEES_LVC))
echo "$REFUND_LVC Lovelace"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --fee $FEES_LVC \
  --tx-in $TX_IN1 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --tx-out $PAYMENT_ADDR1+$REFUND_LVC \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

### 5a2 Unlock the funds in the the script

```
WALLET_ADDR1=$(cat ~/cardano/keys/alonzo/wallets/wallet1.addr)
WALLET_ADDR2=$(cat ~/cardano/keys/alonzo/wallets/wallet2.addr)
PAYMENT_ADDR=${WALLET_ADDR1}
TO_ADDR=${WALLET_ADDR2}

SECRET_VALUE='secret2' \
  && SECRET_JSON=$(printf "{\"bytes\":\"%s\"}" $(echo -n $SECRET_VALUE | xxd -ps)) \
  && echo "$SECRET_JSON" > ~/cardano/scratch/redeemer-secret2.json \
  && echo $SECRET_JSON

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 8

# Query Payment UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic 8

TX_IN1="5f1d1def76880668f70a79965e9e01b058d47deadee4e2386a3c775d7e760b16#0"
TX_IN1_LVC="2000000000"

TX_COL="09a0e6c42682440dbd71cf1db0ddb5b35701d2dc8e8f2aee837db64e71b0ab85#0"
TX_COL_LVC="2000000000"

TX_SCRIPT="8869a7fd2b09021c06f262d4611e5a23b31640b1943db3aaefd79e45b8cd7c4a#0"
TX_SCRIPT_LVC="10000000"

# Calculate the change to send back to PAYMENT_ADDR
ExCPU=29392000
ExMem=7280
ExFct=30
UNITS="($(($ExFct*$ExCPU)),$(($ExFct*$ExMem)))"
FEES_LVC=$(($ExFct * ($ExCPU + $ExMem) + 500000))
REFUND_LVC=$(($TX_IN1_LVC - $FEES_LVC))
SEND_LVC=$TX_SCRIPT_LVC
echo "Send=$SEND_LVC, Refund=$REFUND_LVC, Fees=$FEES_LVC, Units=$UNITS"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --fee $FEES_LVC \
  --tx-in $TX_IN1 \
  --tx-in $TX_SCRIPT \
  --tx-in-script-file /var/cardano/local/scripts/helloworld5a.plutus \
  --tx-in-datum-value 0 \
  --tx-in-redeemer-file /var/cardano/local/scratch/redeemer-secret2.json \
  --tx-in-execution-units "$UNITS" \
  --tx-in-collateral $TX_COL \
  --tx-out $TO_ADDR+$SEND_LVC \
  --tx-out $PAYMENT_ADDR+$REFUND_LVC \
  --protocol-params-file /var/cardano/local/scratch/protocol.json \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/wallet1.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

Check that the funds have been transferred correctly between the wallets.

```
# Query UTxO
cardano-cli query utxo \
  --address $WALLET_ADDR1 \
  --testnet-magic 8 \
&& cardano-cli query utxo \
  --address $WALLET_ADDR2 \
  --testnet-magic 8
```

6. Produce a second transaction that sends some ada from `wallet2.addr` to `wallet1.addr` guarded by a different “secret spending key”.  Practice sending Ada between the “wallets” and observe the effect on the corresponding UTxO entries.

### 6a1 Lock some funds in the script

Wallet2 locks 1000 ADA in the script

```
DATUM_HASH="$(cardano-cli transaction hash-script-data --script-data-value 0)" \
  && DATUM_HASH=${DATUM_HASH:0:64} \
  && echo "DATUM_HASH=\"$DATUM_HASH\""

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 8

# Query UTxO
cardano-cli query utxo \
  --address $WALLET_ADDR2 \
  --testnet-magic 8

# Calculate the change to send back to PAYMENT_ADDR
FEES_LVC=200000
SEND_LVC=10000000
REFUND_LVC=$((2000000000 + 10000000 - SEND_LVC - $FEES_LVC))
echo "$REFUND_LVC Lovelace"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --fee $FEES_LVC \
  --tx-in "09a0e6c42682440dbd71cf1db0ddb5b35701d2dc8e8f2aee837db64e71b0ab85#1" \
  --tx-in "3ada602358619cc701b7420d49cb1a7b7bd6e208a4ba7f1aff79c35261643cd4#0" \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --tx-out $PAYMENT_ADDR1+$REFUND_LVC \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/wallet2.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

### 6.2 Unlock the funds in the the script

Wallet2 sends 1000 ADA from the script to wallet1

```
WALLET_ADDR1=$(cat ~/cardano/keys/alonzo/wallets/wallet1.addr)
WALLET_ADDR2=$(cat ~/cardano/keys/alonzo/wallets/wallet2.addr)

SECRET_VALUE='secret1' \
  && SECRET_JSON=$(printf "{\"bytes\":\"%s\"}" $(echo -n $SECRET_VALUE | xxd -ps)) \
  && echo "$SECRET_JSON" > ~/cardano/scratch/redeemer-secret2.json \
  && echo $SECRET_JSON

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 8

# Query Payment UTxO
cardano-cli query utxo \
  --address $WALLET_ADDR2 \
  --testnet-magic 8

TX_IN1="5f1d1def76880668f70a79965e9e01b058d47deadee4e2386a3c775d7e760b16#2"
TX_IN1_LVC="2000000000"

TX_COL="5f1d1def76880668f70a79965e9e01b058d47deadee4e2386a3c775d7e760b16#3"
TX_COL_LVC="2000000000"

TX_SCRIPT="d0527c31c1c4e502dc933f3769043e2a2dc1c0a2935ddb864bfab68292569fe1#0"
TX_SCRIPT_LVC="10000000"

# Calculate the change to send back to PAYMENT_ADDR
ExCPU=29392000
ExMem=7280
ExFct=30
UNITS="($(($ExFct*$ExCPU)),$(($ExFct*$ExMem)))"
FEES_LVC=$(($ExFct * ($ExCPU + $ExMem) + 500000))
REFUND_LVC=$(($TX_IN1_LVC - $FEES_LVC))
SEND_LVC=$TX_SCRIPT_LVC
echo "Send=$SEND_LVC, Refund=$REFUND_LVC, Fees=$FEES_LVC, Units=$UNITS"

# Build, sign and submit the transaction
cardano-cli transaction build-raw \
  --alonzo-era \
  --fee $FEES_LVC \
  --tx-in $TX_IN1 \
  --tx-in $TX_SCRIPT \
  --tx-in-script-file /var/cardano/local/scripts/helloworld5a.plutus \
  --tx-in-datum-value 0 \
  --tx-in-redeemer-file /var/cardano/local/scratch/redeemer-secret2.json \
  --tx-in-execution-units "$UNITS" \
  --tx-in-collateral $TX_COL \
  --tx-out $WALLET_ADDR1+$SEND_LVC \
  --tx-out $WALLET_ADDR2+$REFUND_LVC \
  --protocol-params-file /var/cardano/local/scratch/protocol.json \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/wallet2.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

Check that the funds have been transferred correctly between the wallets.

```
# Query UTxO
cardano-cli query utxo \
  --address $WALLET_ADDR1 \
  --testnet-magic 8 \
&& cardano-cli query utxo \
  --address $WALLET_ADDR2 \
  --testnet-magic 8
```

7. Optional Exercise (Easy)

Extend your transactions from Q5/6 so that each “wallet” always maintains a minimum balance.

8. Optional Exercise (Moderate)

Produce a Plutus “slot machine” that pays out a Lovelace “jackpot” if it is given a specific set of symbols as a redeemer (e.g. three Bells pays 100 Lovelace).  The machine takes a fixed fee in lovelace and pays any winnings from a pre-funded spending “pot”.  It should not pay out the jackpot if there are insufficient funds in the “pot”. Test your slot machines by exchanging scripts with other testnet users.

9. Optional Exercise (Easy)

Extend the slot machine from Q8 to pay different jackpots for different winning combinations.

The next exercise will involve compiling and submitting some more complex Plutus scripts using your own node.

## Feedback


**Please let us know of any problems that you have encountered**

- Via the Discord channels for general questions.

- Via the issue tracker at [https://github.com/input-output-hk/cardano-node/issues](https://github.com/input-output-hk/cardano-node/issues) for any bugs in the node etc.  Please tag them as Alonzo-related.

- Via the issue tracker at [https://github.com/input-output-hk/plutus/issues](https://github.com/input-output-hk/plutus/issues) for any bugs or feature requests with plutus, playground, PAB etc.

- Via the issue tracker at [https://github.com/input-output-hk/Alonzo-testnet/issues](https://github.com/input-output-hk/Alonzo-testnet/issues) for any issues with the exercises.
