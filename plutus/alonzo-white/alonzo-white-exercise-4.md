## Alonzo White Testnet Exercise Sheet 4: "Compiling and Submitting Simple Plutus Transactions"


In the third exercise, you submitted a pre-compiled transaction to the Alonzo testnet using the node CLI, and made sure you could use the test ada that had been given to you. In this exercise, you will write some very simple Plutus transactions, calculate the fees for those transactions, and submit them to the testnet.

## Prerequisites ##
1. Complete [Exercise 3](3_Alonzo-white-exercise-3.md)
2. Start a passive Cardano node if you need to, and make sure that it has synced with the testnet. It should be in the Alonzo era.
3. Make sure you have some Alonzo White test ada
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
cd ~/git/nessus-cardano/plutus/alonzo-white/plutus-sources/plutus-alwayssucceeds \
  && cabal run plutus-alwayssucceeds 1 alwayssucceeds.plutus \
  && cp alwayssucceeds.plutus ../../plutus-scripts \
  && mv alwayssucceeds.plutus ~/cardano/scripts \
  && cat ~/cardano/scripts/alwayssucceeds.plutus \
  && cardano-cli address build \
    --payment-script-file /var/cardano/local/scripts/alwayssucceeds.plutus \
    --out-file /var/cardano/local/scratch/alwayssucceeds.addr \
    --testnet-magic 7 \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/alwayssucceeds.addr) \
  && echo "SCRIPT_ADDR=${SCRIPT_ADDR}"

ExBudget {_exBudgetCPU = ExCPU 4081000, _exBudgetMemory = ExMemory 790}
SCRIPT_ADDR=addr_test1wz9q37z3kgh9c4x7ppa7xplw4va4epvg4r82svvcvlrcdcqzuzfp7
```

You may also want to inspect the Plutus Core form – this is a representation of the compiled script that will be executed on the blockchain. Confirm that your script is identical to the pre-compiled version that you used in [Exercise 3](3_Alonzo-white-exercise-3.md). If not, how is it different, and why?

2. Compile the `HelloWorld` Plutus script from [source](../../resources/plutus-sources/plutus-helloworld). Save the serialised Plutus script into a file `helloworld.plutus`.

### 2.1 Compile HelloWorld (Static Value)

```
cd ~/git/nessus-cardano/plutus/alonzo-white/plutus-sources/plutus-helloworld \
  && cabal run plutus-helloworld 1 helloworld.plutus \
  && cp helloworld.plutus ../../plutus-scripts \
  && mv helloworld.plutus ~/cardano/scripts \
  && cardano-cli address build \
    --payment-script-file /var/cardano/local/scripts/helloworld.plutus \
    --out-file /var/cardano/local/scratch/helloworld.addr \
    --testnet-magic 7 \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/helloworld.addr) \
  && echo "SCRIPT_ADDR=\"${SCRIPT_ADDR}\""

ExBudget {_exBudgetCPU = ExCPU 9814000, _exBudgetMemory = ExMemory 2260}
SCRIPT_ADDR="addr_test1wpcraj4u9dg7x8kj52a44ja8hn66t4udxyxd9ll9gavqcxsr8w9j9"
```

3. Build a Cardano transaction that will submit `helloworld.plutus` for execution on the testnet. You will need to provide two inputs: one to pay for the transaction and one to provide the collateral for running the Plutus script.

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

# Query Script UTOX
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 7

# Query UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 7

TX_IN1="c3ae965c194b811f4e7fdd447c87e8f656c10c7e0958c8f7efb538c584d65bd1#0"
TX_IN1_LVC="804412200"

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
  --testnet-magic 7
```

### 3.2 Unlock the funds in HelloWorld (Static Value)

```
# Query Script UTOX
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 7

# Query Payment UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 7

TX_IN1="104f7494d648eb7d9bd5de1a74a6a5c0d4483d345962ed75a2d8d5531d8c42a9#0"
TX_IN1_LVC="442715400"

TX_COL="9f1da07ffac8613627c52e378c9e8f4494b23e17df4709b93d47d0021617e153#2"
TX_COL_LVC="1100000000"

TX_SCRIPT="95e447f6142b239c56a267f0e4a002736d2455ee74b0195b5a3b87c0532966e3#0"
TX_SCRIPT_LVC="98000000"

# Calculate the change to send back to PAYMENT_ADDR
ExCPU=9814000
ExMem=2260
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
  --testnet-magic 7
```

4. Modify the `HelloWorld` Plutus script:

a. To succeed if the datum is your name;

### 4a1 Build the validator script

```
cd ~/git/nessus-cardano/plutus/alonzo-white/plutus-sources/plutus-helloworld \
  && cabal run plutus-helloworld4a 1 helloworld4a.plutus \
  && cp helloworld4a.plutus ../../plutus-scripts \
  && mv helloworld4a.plutus ~/cardano/scripts \
  && cardano-cli address build \
    --payment-script-file /var/cardano/local/scripts/helloworld4a.plutus \
    --out-file /var/cardano/local/scratch/helloworld4a.addr \
    --testnet-magic 7 \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/helloworld4a.addr) \
  && echo "SCRIPT_ADDR=\"${SCRIPT_ADDR}\""

ExBudget {_exBudgetCPU = ExCPU 26389000, _exBudgetMemory = ExMemory 6510}
"Datum value: {\"bytes\":\"54686f6d6173\"}"
SCRIPT_ADDR="addr_test1wr3rgp9g3azz0vvcdjt99zqf8ahchrgkmuf4q7a6ljyw47q36c8k2"
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

# Query Script UTOX
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 7

# Query UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 7

TX_IN1="980494b264fd391794245139221d5b74d07872be6b90307b2f49b1a6bba52a9b#0"
TX_IN1_LVC="425069400"

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
  --testnet-magic 7
```

### 4a3 Unlock the funds in the the script

```
# Query Script UTOX
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 7

# Query Payment UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 7

TX_IN1="e72dd064d8003ae38627b979f1f2beb30bfef9ca636e87882f145a2e32600687#1"
TX_IN1_LVC="2000000000"

TX_COL="e72dd064d8003ae38627b979f1f2beb30bfef9ca636e87882f145a2e32600687#2"
TX_COL_LVC="2000000000"

TX_SCRIPT="ef564944e7002891ccb7e20febc36d07173b088bf945dbfc2072d9357027d1e3#0"
TX_SCRIPT_LVC="10000000"

# Calculate the change to send back to PAYMENT_ADDR
ExCPU=26389000
ExMem=6510
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
  --testnet-magic 7
```

### 4b1 Build the validator script

b. To succeed if the redeemer is also your birthday;

```
cd ~/git/nessus-cardano/plutus/alonzo-white/plutus-sources/plutus-helloworld \
  && cabal run plutus-helloworld4b 1 helloworld4b.plutus \
  && cp helloworld4b.plutus ../../plutus-scripts \
  && mv helloworld4b.plutus ~/cardano/scripts \
  && cardano-cli address build \
    --payment-script-file /var/cardano/local/scripts/helloworld4b.plutus \
    --out-file /var/cardano/local/scratch/helloworld4b.addr \
    --testnet-magic 7 \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/helloworld4b.addr) \
  && echo "SCRIPT_ADDR=\"${SCRIPT_ADDR}\""

ExBudget {_exBudgetCPU = ExCPU 26857000, _exBudgetMemory = ExMemory 6630}
"Datum value: {\"bytes\":\"4865696e7a\"}"
SCRIPT_ADDR="addr_test1wque26j6xdnq2fmt90akjneqc4n2v887c3vqf4fmyl8zndqh2rps7"
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

# Query Script UTOX
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 7

# Query UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 7

TX_IN1="4bcce13d65305edf4aab619159d67b26e226d46d0952001d4500fc42c7367035#0"
TX_IN1_LVC="1217634700"

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
  --testnet-magic 7
```

### 4b3 Unlock the funds in the the script

```
# Query Script UTOX
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 7

# Query Payment UTOX
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 7

TX_IN1="1f7768d23f50e10cf8ae3beaec9f12ac4e195f67ef6b4addeb451f665528ef25#1"
TX_IN1_LVC="1207434700"

TX_COL="e72dd064d8003ae38627b979f1f2beb30bfef9ca636e87882f145a2e32600687#2"
TX_COL_LVC="2000000000"

TX_SCRIPT="1f7768d23f50e10cf8ae3beaec9f12ac4e195f67ef6b4addeb451f665528ef25#0"
TX_SCRIPT_LVC="10000000"

# Calculate the change to send back to PAYMENT_ADDR
ExCPU=26857000
ExMem=6630
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
  --testnet-magic 7
```

c. To take a datum and a redeemer and to succeed if the redeemer is the same as the datum;
d. To take a datum that represents a pair of integers and a redeemer that represents a list of integers and succeed if all elements in the redeemer are within the range specified by the values in the datum.

Compile each of the Plutus transactions, build the corresponding Cardano transactions, and submit these to the blockchain.  Test your transactions on various inputs.

5. Set up three new payment addresses: `payment.addr`, `wallet1.addr`, and `wallet2.addr` using the node CLI commands.  Transfer some ada to each of these addresses, and check that they have been funded.

Produce a transaction that sends 100 ada from `wallet1.addr` to `wallet2.addr` provided the correct “secret spending key” is provided as a redeemer.

Determine the cost and required collateral for the Cardano transaction using the CLI commands and then construct and submit the transaction, paying the exact cost and collateral.

`cardano-cli transaction build-raw …`
`cardano-cli transaction calculate-min-fee …`
`…`

Check that the funds have been transferred correctly between the wallets.

6. Produce a second transaction that sends some ada from `wallet2.addr` to `wallet1.addr` guarded by a different “secret spending key”.  Practice sending Ada between the “wallets” and observe the effect on the corresponding UTxO entries.

`cardano-cli query utxo …`

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

- Via the issue tracker at [https://github.com/input-output-hk/Alonzo-testnet/issues](https://github.com/input-output-hk/Alonzo-testnet/issues) for any issues with the exercises.
