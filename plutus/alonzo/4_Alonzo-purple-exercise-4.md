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
cd ~/git/nessus-cardano/plutus/alonzo/plutus-sources/plutus-alwayssucceeds \
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
cd ~/git/nessus-cardano/plutus/alonzo/plutus-sources/plutus-helloworld \
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

TX_IN1="8869a7fd2b09021c06f262d4611e5a23b31640b1943db3aaefd79e45b8cd7c4a#1"

SEND_LVC=100000000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --change-address $PAYMENT_ADDR1 \
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

TX_SCRIPT="74de20abf20fde79fa02215369764339fad60bef5a923b972140e103d9f50f4b#1"

TX_IN1="74de20abf20fde79fa02215369764339fad60bef5a923b972140e103d9f50f4b#0"

TX_COL="1479fbe7e80e672e6c75897185dca0ff6771e17d91cd7da175694e4b5b1c24e0#3"

SEND_LVC=100000000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-in $TX_SCRIPT \
  --tx-in-datum-value $DATUM_VALUE \
  --tx-in-redeemer-value 0 \
  --tx-in-script-file /var/cardano/local/scripts/helloworld.plutus \
  --tx-in-collateral $TX_COL \
  --tx-out $PAYMENT_ADDR1+$SEND_LVC \
  --change-address $PAYMENT_ADDR1 \
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
cd ~/git/nessus-cardano/plutus/alonzo/plutus-sources/plutus-helloworld \
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

TX_IN1="9a400f4711e06c2fe2e1d864dc657378600b2b80af9b353a10245696362dba21#1"

SEND_LVC=10000000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --change-address $PAYMENT_ADDR1 \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

### 4a3 Unlock the funds in the script

```
# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 8

# Query Payment UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 8

TX_SCRIPT="07190bd04288987dc2058a0dd52c202716601d9853bed6cea2fdbb0ba2ed434c#1"

TX_IN1="07190bd04288987dc2058a0dd52c202716601d9853bed6cea2fdbb0ba2ed434c#0"

TX_COL="1479fbe7e80e672e6c75897185dca0ff6771e17d91cd7da175694e4b5b1c24e0#2"

SEND_LVC=10000000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-in $TX_SCRIPT \
  --tx-in-datum-file /var/cardano/local/scratch/script-datum.json \
  --tx-in-redeemer-value '""' \
  --tx-in-script-file /var/cardano/local/scripts/helloworld4a.plutus \
  --tx-in-collateral $TX_COL \
  --tx-out $PAYMENT_ADDR1+$SEND_LVC \
  --change-address $PAYMENT_ADDR1 \
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
cd ~/git/nessus-cardano/plutus/alonzo/plutus-sources/plutus-helloworld \
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

TX_IN1="30603e9d409236bb5f64ecde37ad1f1110b0e5aafb4998f18f1b9d4b81ec9025#0"

SEND_LVC=10000000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --change-address $PAYMENT_ADDR1 \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

### 4b3 Unlock the funds in the script

```
# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 8

# Query Payment UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 8

TX_SCRIPT="77203d68be18b8288805db3e67294636db65be7f9e28a0779d5c34b530f842e8#1"

TX_IN1="77203d68be18b8288805db3e67294636db65be7f9e28a0779d5c34b530f842e8#0"

TX_COL="1479fbe7e80e672e6c75897185dca0ff6771e17d91cd7da175694e4b5b1c24e0#3"

SEND_LVC=10000000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-in $TX_SCRIPT \
  --tx-in-datum-file /var/cardano/local/scratch/script-datum.json \
  --tx-in-redeemer-value 29031918 \
  --tx-in-script-file /var/cardano/local/scripts/helloworld4b.plutus \
  --tx-in-collateral $TX_COL \
  --change-address $PAYMENT_ADDR1 \
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
cd ~/git/nessus-cardano/plutus/alonzo/plutus-sources/plutus-helloworld \
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

TX_IN1="9a400f4711e06c2fe2e1d864dc657378600b2b80af9b353a10245696362dba21#0"

SEND_LVC=10000000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --change-address $PAYMENT_ADDR1 \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

### 4d3 Unlock the funds in the script

```
# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 8

# Query Payment UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 8

TX_SCRIPT="06bd7251809e35f273af283a6a754c41180fd543a83a0d913d1db254a2fc1322#1"

TX_IN1="cd871ba907ae54a18ae1033dd0a54645348331bfda7f7aaf7e83c0483f6b9953#0"

TX_COL="1479fbe7e80e672e6c75897185dca0ff6771e17d91cd7da175694e4b5b1c24e0#3"

SEND_LVC=10000000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-in $TX_SCRIPT \
  --tx-in-datum-value '[1,5]' \
  --tx-in-redeemer-value '[1,2,3,4]' \
  --tx-in-script-file /var/cardano/local/scripts/helloworld4d.plutus \
  --tx-in-collateral $TX_COL \
  --change-address $PAYMENT_ADDR1 \
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

### 5.1 Set up new payment addresses for wallet1 & wallet2

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

TX_IN1="ed176363ec67f8cc0bedf3e7cfeee8db528c5ff412e42fdbe0237f9173a7dc55#0"

SEND_LVC=2000000000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-out $WALLET_ADDR1+$SEND_LVC \
  --tx-out $WALLET_ADDR1+$SEND_LVC \
  --tx-out $WALLET_ADDR1+$SEND_LVC \
  --tx-out $WALLET_ADDR2+$SEND_LVC \
  --tx-out $WALLET_ADDR2+$SEND_LVC \
  --tx-out $WALLET_ADDR2+$SEND_LVC \
  --change-address $PAYMENT_ADDR0 \
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

### 5.2 Produce a transaction that sends 100 ada from `wallet1.addr` to `wallet2.addr`

Produce a transaction that sends 100 ada from `wallet1.addr` to `wallet2.addr` provided the correct “secret spending key” is provided as a redeemer and submit this.

```
cd ~/git/nessus-cardano/plutus/alonzo/plutus-sources/plutus-helloworld \
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

### 5.3 Lock some funds in the script

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

TX_IN1="ad52994300ff23f20143368271e9764094724900bf77a86f5545e7a63052a17b#0"

SEND_LVC=10000000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --change-address $PAYMENT_ADDR1 \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

### 5.4 Unlock the funds in the script

```
WALLET_ADDR1=$(cat ~/cardano/keys/alonzo/wallets/wallet1.addr)
WALLET_ADDR2=$(cat ~/cardano/keys/alonzo/wallets/wallet2.addr)
PAYMENT_ADDR=${WALLET_ADDR1}
TO_ADDR=${WALLET_ADDR2}

SECRET_VALUE='secret1' \
  && SECRET_JSON=$(printf "{\"bytes\":\"%s\"}" $(echo -n $SECRET_VALUE | xxd -ps)) \
  && echo "$SECRET_JSON" > ~/cardano/scratch/redeemer-secret1.json \
  && echo $SECRET_JSON

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 8

# Query Payment UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic 8

TX_SCRIPT="dc17f12d92e6cd94279f84800933e6074d4c338373d30d2125d9f6c7fba06f10#1"

TX_IN1="5f1d1def76880668f70a79965e9e01b058d47deadee4e2386a3c775d7e760b16#1"

TX_COL="09a0e6c42682440dbd71cf1db0ddb5b35701d2dc8e8f2aee837db64e71b0ab85#0"

SEND_LVC=10000000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-in $TX_SCRIPT \
  --tx-in-datum-value 0 \
  --tx-in-redeemer-file /var/cardano/local/scratch/redeemer-secret1.json \
  --tx-in-script-file /var/cardano/local/scripts/helloworld5a.plutus \
  --tx-in-collateral $TX_COL \
  --tx-out $TO_ADDR+$SEND_LVC \
  --change-address $PAYMENT_ADDR \
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

### 6.1 Produce a second transaction that sends some ada from `wallet2.addr` to `wallet1.addr` guarded by a different “secret spending key”.  

Practice sending Ada between the “wallets” and observe the effect on the corresponding UTxO entries.

### 6.2 Lock some funds in the script

Wallet2 locks 1000 ADA in the script

```
WALLET_ADDR1=$(cat ~/cardano/keys/alonzo/wallets/wallet1.addr)
WALLET_ADDR2=$(cat ~/cardano/keys/alonzo/wallets/wallet2.addr)
PAYMENT_ADDR=${WALLET_ADDR2}

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

SEND_LVC=10000000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in "36944b03d640b6022f0773f78d3eed3f18da0e8006db0387ffa46fb8766997ae#0" \
  --tx-in "66da08eca83f74fad4ac453b1db7633db0a48b6e4a6c7d393daa206275a24cdb#0" \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --change-address $PAYMENT_ADDR \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/wallet2.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

### 6.3 Unlock the funds in the script

Wallet2 sends 1000 ADA from the script to wallet1

```
WALLET_ADDR1=$(cat ~/cardano/keys/alonzo/wallets/wallet1.addr)
WALLET_ADDR2=$(cat ~/cardano/keys/alonzo/wallets/wallet2.addr)
PAYMENT_ADDR=${WALLET_ADDR2}
TO_ADDR=${WALLET_ADDR1}

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
  --address $WALLET_ADDR2 \
  --testnet-magic 8

TX_SCRIPT="6da937e57b4b49921e10d293bb3236f5bf2bb1512248ac4492dfd05eeaea2c75#1"

TX_IN1="d604bf2e1da2fb1b69db58be9e0e92c5ff8fb7d32133ab65f901d4fd350a00fc#4"

TX_COL="5f1d1def76880668f70a79965e9e01b058d47deadee4e2386a3c775d7e760b16#3"

SEND_LVC=10000000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-in $TX_SCRIPT \
  --tx-in-datum-value 0 \
  --tx-in-redeemer-file /var/cardano/local/scratch/redeemer-secret2.json \
  --tx-in-script-file /var/cardano/local/scripts/helloworld5a.plutus \
  --tx-in-collateral $TX_COL \
  --tx-out $TO_ADDR+$SEND_LVC \
  --change-address $PAYMENT_ADDR \
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

### 7 Optional Exercise (Easy)

Extend your transactions from Q5/6 so that each “wallet” always maintains a minimum balance.

### 8 Optional Exercise (Moderate)

Produce a Plutus “slot machine” that pays out a Lovelace “jackpot” if it is given a specific set of symbols as a redeemer (e.g. three Bells pays 100 Lovelace).  The machine takes a fixed fee in lovelace and pays any winnings from a pre-funded spending “pot”.  It should not pay out the jackpot if there are insufficient funds in the “pot”. Test your slot machines by exchanging scripts with other testnet users.

### 9 Optional Exercise (Easy)

Extend the slot machine from Q8 to pay different jackpots for different winning combinations.

The next exercise will involve compiling and submitting some more complex Plutus scripts using your own node.

## Feedback

**Please let us know of any problems that you have encountered**

- Via the Discord channels for general questions.

- Via the issue tracker at [https://github.com/input-output-hk/cardano-node/issues](https://github.com/input-output-hk/cardano-node/issues) for any bugs in the node etc.  Please tag them as Alonzo-related.

- Via the issue tracker at [https://github.com/input-output-hk/plutus/issues](https://github.com/input-output-hk/plutus/issues) for any bugs or feature requests with plutus, playground, PAB etc.

- Via the issue tracker at [https://github.com/input-output-hk/Alonzo-testnet/issues](https://github.com/input-output-hk/Alonzo-testnet/issues) for any issues with the exercises.
