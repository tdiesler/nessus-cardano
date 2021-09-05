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

2. Compile the `HelloWorld` Plutus script from [source](../../resources/plutus-sources/plutus-helloworld).
Save the serialised Plutus script into a file `helloworld.plutus`.

### 2.1 Compile HelloWorld (Static Value)

```
cd ~/git/nessus-cardano/plutus/alonzo/plutus-sources/plutus-helloworld \
  && cabal run plutus-helloworld 1 helloworld.plutus \
  && cp helloworld.plutus ../../plutus-scripts \
  && mv helloworld.plutus ~/cardano/scripts \
  && cardano-cli address build \
    --testnet-magic $TESTNET_MAGIC \
    --payment-script-file /var/cardano/local/scripts/helloworld.plutus \
    --out-file /var/cardano/local/scratch/helloworld.addr \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/helloworld.addr) \
  && echo "SCRIPT_ADDR=\"${SCRIPT_ADDR}\""

ExBudget {exBudgetCPU = ExCPU 1191020, exBudgetMemory = ExMemory 4100}
SCRIPT_ADDR="addr_test1wrs527svgl0m0ghkhramqkdae643v0q96d83jku8h8etxrs58smpj"
```

3. Build a Cardano transaction that will submit `helloworld.plutus` for execution on the testnet.  As before, you will need to provide two input addresses: one to pay for the transaction fees and one to provide the collateral that is spent if the Plutus script fails to validate (to avoid risking all your funds, it is recommended that you use a dedicated payment address with limited funds to pay for the collateral rather than your main payment address!).

### 3.1 Lock some funds in HelloWorld (Static Value)

We create a UTxO locked by the script that holds an integer datum hash.

```
DATUM_HEX=$(printf "0x%s" $(echo -n 'Hello!' | xxd -ps)) \
  && echo -e "$DATUM_HEX" \
  && DATUM_VALUE=$(printf "%d" $DATUM_HEX) \
  && echo $DATUM_VALUE \
  && DATUM_HASH="$(cardano-cli transaction hash-script-data --script-data-value $DATUM_VALUE)" \
  && DATUM_HASH=${DATUM_HASH:0:64} \
  && echo "DATUM_HASH=\"$DATUM_HASH\""

# [BUG] - 'cardano-cli transaction hash-script-data' appends whitespace
# https://github.com/input-output-hk/cardano-node/issues/2937

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic $TESTNET_MAGIC

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN="4a048c7609e7d977f2a978c5f91efe074cb9e9bf86e6a51eb52fb67a9899d264#0"

SEND_LVC=12300000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --change-address $PAYMENT_ADDR1 \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

### 3.2 Unlock the funds in HelloWorld (Static Value)

```
# Query Payment UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic $TESTNET_MAGIC

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN="2d86a1baea640bf02d2aa5ee111da804acc59b12df155a869d20ac5de32f2f79#0"

TX_SCRIPT="2d86a1baea640bf02d2aa5ee111da804acc59b12df155a869d20ac5de32f2f79#1"

TX_COL="689abea1c788f21f191aa2f05cf8367687d0dff3f514cc41a3af8e9f2a0d5d8b#4"

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN \
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
  --signing-key-file /var/cardano/local/keys/testnet/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
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
    --testnet-magic $TESTNET_MAGIC \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/helloworld4a.addr) \
  && echo "SCRIPT_ADDR=\"${SCRIPT_ADDR}\""

ExBudget {exBudgetCPU = ExCPU 8544951, exBudgetMemory = ExMemory 28800}
"Datum value: {\"bytes\":\"54686f6d6173\"}"
SCRIPT_ADDR="addr_test1wranjjzwgr86vdk4myd3uy4z7wnh4t6sr9xrgg6nfuwzjwgh5k275"
```

### 4a2 Lock some funds in the script

```
DATUM_VALUE='Thomas' \
  && DATUM_JSON=$(printf "{\"bytes\":\"%s\"}" $(echo -n $DATUM_VALUE | xxd -ps)) \
  && echo "$DATUM_JSON" > ~/cardano/scratch/script-datum.json \
  && echo $DATUM_JSON \
  && DATUM_HASH="$(cardano-cli transaction hash-script-data --script-data-file /var/cardano/local/scratch/script-datum.json)" \
  && DATUM_HASH=${DATUM_HASH:0:64} \
  && echo "DATUM_HASH=\"$DATUM_HASH\""

# [BUG] - 'cardano-cli transaction hash-script-data' appends whitespace
# https://github.com/input-output-hk/cardano-node/issues/2937

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic $TESTNET_MAGIC

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="b63e54e08f22292d3fa930723d38a7ac791c0149dd353ace1c16c592161dddaf#0"
TX_IN2="b63e54e08f22292d3fa930723d38a7ac791c0149dd353ace1c16c592161dddaf#1"

SEND_LVC=12300000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN1 \
  --tx-in $TX_IN2 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --change-address $PAYMENT_ADDR1 \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

### 4a3 Unlock the funds in the script

```
# Query Payment UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic $TESTNET_MAGIC

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN="54249401e0955d441133919d6b671069482d34491ff68d8b1e0c6c4b2fc19f9b#0"

TX_SCRIPT="54249401e0955d441133919d6b671069482d34491ff68d8b1e0c6c4b2fc19f9b#1"

TX_COL="689abea1c788f21f191aa2f05cf8367687d0dff3f514cc41a3af8e9f2a0d5d8b#4"

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN \
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
  --signing-key-file /var/cardano/local/keys/testnet/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
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
    --testnet-magic $TESTNET_MAGIC \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/helloworld4b.addr) \
  && echo "SCRIPT_ADDR=\"${SCRIPT_ADDR}\""

ExBudget {exBudgetCPU = ExCPU 8723589, exBudgetMemory = ExMemory 29400}
"Datum value: {\"bytes\":\"4865696e7a\"}"
SCRIPT_ADDR="addr_test1wpf7yjjdfm7jnv5q3yf9s9g96huqvu7qmawp6kv6w0gw9zgcldhkf"
```

### 4b2 Lock some funds in the script

```
DATUM_VALUE='Heinz' \
  && DATUM_JSON=$(printf "{\"bytes\":\"%s\"}" $(echo -n $DATUM_VALUE | xxd -ps)) \
  && echo "$DATUM_JSON" > ~/cardano/scratch/script-datum.json \
  && echo $DATUM_JSON \
  && DATUM_HASH="$(cardano-cli transaction hash-script-data --script-data-file /var/cardano/local/scratch/script-datum.json)" \
  && DATUM_HASH=${DATUM_HASH:0:64} \
  && echo "DATUM_HASH=\"$DATUM_HASH\""

# [BUG] - 'cardano-cli transaction hash-script-data' appends whitespace
# https://github.com/input-output-hk/cardano-node/issues/2937

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic $TESTNET_MAGIC

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="f633624c09ef1f2157e29f8960467d091886c02d1e2e386dd9ec600e754d96ff#0"
TX_IN2="f633624c09ef1f2157e29f8960467d091886c02d1e2e386dd9ec600e754d96ff#1"

SEND_LVC=12300000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN1 \
  --tx-in $TX_IN2 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --change-address $PAYMENT_ADDR1 \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

### 4b3 Unlock the funds in the script

```
# Query Payment UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic $TESTNET_MAGIC

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN="8847a420c405f694c8c716699f590c7b3ad6b3078d6cd6e04afe1df41bf0e128#0"

TX_SCRIPT="8847a420c405f694c8c716699f590c7b3ad6b3078d6cd6e04afe1df41bf0e128#1"

TX_COL="689abea1c788f21f191aa2f05cf8367687d0dff3f514cc41a3af8e9f2a0d5d8b#4"

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN \
  --tx-in $TX_SCRIPT \
  --tx-in-datum-file /var/cardano/local/scratch/script-datum.json \
  --tx-in-redeemer-value 29031918 \
  --tx-in-script-file /var/cardano/local/scripts/helloworld4b.plutus \
  --tx-in-collateral $TX_COL \
  --tx-out $PAYMENT_ADDR1+$SEND_LVC \
  --change-address $PAYMENT_ADDR1 \
  --protocol-params-file /var/cardano/local/scratch/protocol.json \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
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
    --testnet-magic $TESTNET_MAGIC \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/helloworld4d.addr) \
  && echo "SCRIPT_ADDR=\"${SCRIPT_ADDR}\""

ExBudget {exBudgetCPU = ExCPU 10926791, exBudgetMemory = ExMemory 36800}
SCRIPT_ADDR="addr_test1wr5qar5sh0xl3xtjn0ku8pev7yywunuxet9v4sd3gx8ap3cpnmfse"
```

### 4d2 Lock some funds in the script

```
DATUM_VALUE='[1,5]' \
  && DATUM_HASH="$(cardano-cli transaction hash-script-data --script-data-value $DATUM_VALUE)" \
  && DATUM_HASH=${DATUM_HASH:0:64} \
  && echo "DATUM_HASH=\"$DATUM_HASH\""

# [BUG] - 'cardano-cli transaction hash-script-data' appends whitespace
# https://github.com/input-output-hk/cardano-node/issues/2937

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic $TESTNET_MAGIC

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="78a1b8c54bd7bff436201307d21ecf6f90a4c9c13a030db7bcd1e87cf88449d1#0"

SEND_LVC=12300000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN1 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --change-address $PAYMENT_ADDR1 \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

### 4d3 Unlock the funds in the script

```
# Query Payment UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic $TESTNET_MAGIC

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN="9368fd9f667686a0851cd753447a3c95208514ff556e8037695b5c2eef707942#0"

TX_SCRIPT="9368fd9f667686a0851cd753447a3c95208514ff556e8037695b5c2eef707942#1"

TX_COL="689abea1c788f21f191aa2f05cf8367687d0dff3f514cc41a3af8e9f2a0d5d8b#4"

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN \
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
  --signing-key-file /var/cardano/local/keys/testnet/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

### 5.1 Set up new payment addresses for wallet1 & wallet2

Set up three new payment addresses: `payment.addr`, `wallet1.addr`, and `wallet2.addr` using the node CLI commands.  

```
WALLET_ADDR1=${PAYMENT_ADDR1}
WALLET_ADDR2=${PAYMENT_ADDR2}
WALLET_ADDR3=${PAYMENT_ADDR3}

# Extract the PubKey Hashes
cardano-cli address key-hash \
  --payment-verification-key-file /var/cardano/local/keys/testnet/acc1/payment.vkey \
  --out-file /var/cardano/local/scratch/wallet1-pubkey.hash \
&& cardano-cli address key-hash \
  --payment-verification-key-file /var/cardano/local/keys/testnet/acc2/payment.vkey \
  --out-file /var/cardano/local/scratch/wallet2-pubkey.hash \
&& cardano-cli address key-hash \
  --payment-verification-key-file /var/cardano/local/keys/testnet/acc3/payment.vkey \
  --out-file /var/cardano/local/scratch/wallet3-pubkey.hash \
&& WALLET_HASH1="$(cat ~/cardano/scratch/wallet1-pubkey.hash)" \
&& WALLET_HASH2="$(cat ~/cardano/scratch/wallet2-pubkey.hash)" \
&& WALLET_HASH3="$(cat ~/cardano/scratch/wallet3-pubkey.hash)" \
&& echo "WALLET_HASH1=$WALLET_HASH1" \
&& echo "WALLET_HASH2=$WALLET_HASH2" \
&& echo "WALLET_HASH3=$WALLET_HASH3"

# Copy the PubKeyHashes to HelloWorld5a
globalMapping :: [(PubKeyHash, P.BuiltinByteString)]
globalMapping = [
  ("36deb53fa63e507df19b5cd69bc1f0d2a214e3d738b68883fb27e10f", "secret1"),
  ("071c6180a8fd2b486b9f40a1363ac0717518ab305ec3db54f5268ae8", "secret2"),
  ("f9d84b9ee7f753478b5b86be5fadfa8a2b1144ba087bc1b8a1023d18", "secret3")]
```

Transfer some ada to each of these addresses, and check that they have been funded.

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
    --testnet-magic $TESTNET_MAGIC \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/helloworld5a.addr) \
  && echo "SCRIPT_ADDR=${SCRIPT_ADDR}"

ExBudget {exBudgetCPU = ExCPU 13963637, exBudgetMemory = ExMemory 47000}
SCRIPT_ADDR=addr_test1wzcfdx59l5q2r0dffglskzsw4u2wtxfyrx7grp0qkxqusxcjpcfk8
```

### 5.3 Lock some funds in the script

```
DATUM_HASH="$(cardano-cli transaction hash-script-data --script-data-value 0)" \
  && DATUM_HASH=${DATUM_HASH:0:64} \
  && echo "DATUM_HASH=\"$DATUM_HASH\""

# Query UTxO
cardano-cli query utxo \
  --address $WALLET_ADDR1 \
  --testnet-magic $TESTNET_MAGIC

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="cf156590e875d4b91fdb9624a08199b521bbcf8f0c2b1e634bc2c9b81c50ea99#0"

SEND_LVC=12300000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN1 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --change-address $PAYMENT_ADDR1 \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

### 5.4 Unlock the funds in the script

```
PAYMENT_ADDR=${WALLET_ADDR1}
TO_ADDR=${WALLET_ADDR2}

SECRET_VALUE='secret1' \
  && SECRET_JSON=$(printf "{\"bytes\":\"%s\"}" $(echo -n $SECRET_VALUE | xxd -ps)) \
  && echo "$SECRET_JSON" > ~/cardano/scratch/redeemer-secret1.json \
  && echo $SECRET_JSON

# Query Payment UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic $TESTNET_MAGIC

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="9ed93d4df4521830431515547e5b44c7dd46e517221d8148a1f1a8e89fd46767#0"

TX_SCRIPT="9ed93d4df4521830431515547e5b44c7dd46e517221d8148a1f1a8e89fd46767#1"

TX_COL="689abea1c788f21f191aa2f05cf8367687d0dff3f514cc41a3af8e9f2a0d5d8b#4"

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
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
  --signing-key-file /var/cardano/local/keys/testnet/acc1/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

Check that the funds have been transferred correctly between the wallets.

```
# Query UTxO
cardano-cli query utxo \
  --address $WALLET_ADDR1 \
  --testnet-magic $TESTNET_MAGIC \
&& cardano-cli query utxo \
  --address $WALLET_ADDR2 \
  --testnet-magic $TESTNET_MAGIC
```

### 6.1 Produce a second transaction that sends some ada from `wallet2.addr` to `wallet1.addr` guarded by a different “secret spending key”.  

Practice sending Ada between the “wallets” and observe the effect on the corresponding UTxO entries.

### 6.2 Lock some funds in the script

Wallet2 locks 1000 ADA in the script

```
DATUM_HASH="$(cardano-cli transaction hash-script-data --script-data-value 0)" \
  && DATUM_HASH=${DATUM_HASH:0:64} \
  && echo "DATUM_HASH=\"$DATUM_HASH\""

# Query UTxO
cardano-cli query utxo \
  --address $WALLET_ADDR2 \
  --testnet-magic $TESTNET_MAGIC

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="b8413b62fa9d28a9c8b8674378253128209b11df9868eb7a47a5a03642287fc0#1"
TX_IN2="5bfe11d76714f03158fd3458563cc3e66d2dabe3b15ba8f6f7d23bfcd2a4b442#1"

SEND_LVC=12300000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN1 \
  --tx-in $TX_IN2 \
  --tx-out $SCRIPT_ADDR+$SEND_LVC \
  --tx-out-datum-hash $DATUM_HASH \
  --change-address $PAYMENT_ADDR2 \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc2/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

### 6.3 Unlock the funds in the script

Wallet2 sends 1000 ADA from the script to wallet1

```
PAYMENT_ADDR=${WALLET_ADDR2}
TO_ADDR=${WALLET_ADDR1}

SECRET_VALUE='secret2' \
  && SECRET_JSON=$(printf "{\"bytes\":\"%s\"}" $(echo -n $SECRET_VALUE | xxd -ps)) \
  && echo "$SECRET_JSON" > ~/cardano/scratch/redeemer-secret2.json \
  && echo $SECRET_JSON

# Query Payment UTxO
cardano-cli query utxo \
  --address $WALLET_ADDR2 \
  --testnet-magic $TESTNET_MAGIC

# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="264514cc0d6bc563c84513fb4bd02fa4eb68eb053aebb50343b10848341857a2#0"

TX_SCRIPT="264514cc0d6bc563c84513fb4bd02fa4eb68eb053aebb50343b10848341857a2#1"

TX_COL="5bfe11d76714f03158fd3458563cc3e66d2dabe3b15ba8f6f7d23bfcd2a4b442#2"

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
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
  --signing-key-file /var/cardano/local/keys/testnet/acc2/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

Check that the funds have been transferred correctly between the wallets.

```
# Query UTxO
cardano-cli query utxo \
  --address $WALLET_ADDR1 \
  --testnet-magic $TESTNET_MAGIC \
&& cardano-cli query utxo \
  --address $WALLET_ADDR2 \
  --testnet-magic $TESTNET_MAGIC
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
