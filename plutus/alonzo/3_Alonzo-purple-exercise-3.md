# Alonzo Purple Testnet Exercise Sheet 3 "Submitting Transactions Containing Basic Plutus Scripts"

In the first exercise, you set up and ran a passive Cardano node that was connected to the Alonzo Purple testnet.  You may also have participated in the Alonzo hard fork in the optional [Exercise 2](2_Alonzo-purple-exercise-2.md).  In this exercise, you will build and submit transactions to the Testnet that contain pre-compiled Plutus scripts and use the node CLI to manage the test ada that you have obtained via payment addresses and UTxOs.

## Prerequisites

- Complete [Exercise Sheet 1](1_Alonzo-purple-exercise-1.md)
- Ensure that you have the correctly tagged version of the node and CLI
- Make sure you have some Alonzo Purple test ada
- Read the tutorial information on:
	- Payment addresses
	- How to build and submit a Cardano transaction
- You may also want to watch [Plutus Pioneer Program - Lecture #2](https://youtu.be/E5KRk5y9KjQ) for background on Plutus scripts

## Objectives

In this set of exercises, we will make sure that you can:

- Create new Payment Addresses and the Associated Private and Public Keys
- Use Payment Addresses to Fund Private “Wallets”
- Build and Submit Transactions to the Testnet Blockchain, including ones containing Plutus scripts.
- Spend funds that are controlled by the script address.

This is the core of what is required to execute Plutus scripts on the Cardano blockchain.

## Exercises

### Part 2:  Submit a transaction to lock funds.

We will first use a pre-built Plutus validator script that always returns `True`. This is the simplest possible validator script (though it is not very useful except as a placeholder/test script!).

1. Download the pre-built [AlwaysSucceeds.plutus](/resources/plutus-scripts/AlwaysSucceeds.plutus) Plutus script, and obtain the script address.

```
cd ~/git/nessus-cardano/plutus/alonzo/plutus-sources/plutus-alwayssucceeds \
  && cabal run plutus-alwayssucceeds 1 AlwaysSucceeds.plutus \
  && cp AlwaysSucceeds.plutus ~/cardano/scripts \
  && mv AlwaysSucceeds.plutus ../../plutus-scripts/ \
  && cat ~/cardano/scripts/AlwaysSucceeds.plutus \
  && cardano-cli address build \
    --payment-script-file /var/cardano/local/scripts/AlwaysSucceeds.plutus \
    --out-file /var/cardano/local/scratch/alwayssucceeds.addr \
    --testnet-magic 8 \
  && SCRIPT_ADDR=$(cat ~/cardano/scratch/alwayssucceeds.addr) \
  && echo "SCRIPT_ADDR=\"${SCRIPT_ADDR}\""

ExBudget {_exBudgetCPU = ExCPU 1390000, _exBudgetMemory = ExMemory 100}
{
    "type": "PlutusScriptV1",
    "description": "",
    "cborHex": "4e4d01000033222220051200120011"
}
SCRIPT_ADDR="addr_test1wpnlxv2xv9a9ucvnvzqakwepzl9ltx7jzgm53av2e9ncv4sysemm8"
```

### 2.2 Lock some funds in AlwaysSucceeds

All transaction outputs that are locked by non-native scripts must include
the hash of an additional “datum”. **A non-native script-locked output that does not include a datum hash is unspendable**

```
PAYMENT_ADDR1=$(cat ~/cardano/keys/alonzo/acc1/payment.addr)

DATUM_VALUE=29031918 \
  && DATUM_HASH="$(cardano-cli transaction hash-script-data --script-data-value $DATUM_VALUE)" \
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

TX_IN1="18d0cc0a514ead01ce3ad525536e8a838f8375dcf80e790b1763b3f1fb8f0ec1#0"
TX_IN1_LVC="1945766300"

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
  --testnet-magic 8
```

### Part 3:  Unlocking funds that are guarded by a Plutus script.

Now we want to create a transaction to return the locked funds. This validation script always passes but it is still important to use a datum value that matches the datum hash you used in part 2.

* The exact budget to run a script is expressed in terms of computational resources, and
included in the transaction data as `--tx-in-execution-units`

* The exact total fee a transaction is paying is also specified in the transaction data. For a transaction to be valid, this fee must cover the script-running resource budget at the current price, as well as the size-based part of the required fee. If the fee is not sufficient to
cover the resource budget specified (eg. if the resource price increased), the transaction is considered invalid and will not appear on the ledger (will not be included in a valid block). No fees will be collected in this case. This is in contrast with the gas model, where, if prices go
up, a greater fee will be charged - up to the maximum available funds, even if they are not sufficient to cover the cost of the execution of the contract.

* The user specifies the UTxO entries containing funds sufficient to cover a percentage (usually 100 or more) of the total transaction fee. These inputs are only collected in the case of script validation failure, and are called collateral inputs `--tx-in-collateral`. In the case of script validation success, the fee specified in the fee field of the transaction is collected, but the collateral is not.

```
# Query Script UTxO
cardano-cli query utxo \
  --address $SCRIPT_ADDR \
  --testnet-magic 8

# Query Payment UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR1 \
  --testnet-magic 8

TX_IN1="c569575d69a81eff3cd57061da657ebf27e4b5e6fd9fbd81b5d6eb9562e4a620#1"
TX_IN1_LVC="1845566300"

TX_COL="1479fbe7e80e672e6c75897185dca0ff6771e17d91cd7da175694e4b5b1c24e0#3"
TX_COL_LVC="2000000000"

TX_SCRIPT="c569575d69a81eff3cd57061da657ebf27e4b5e6fd9fbd81b5d6eb9562e4a620#0"
TX_SCRIPT_LVC="100000000"

# Calculate the change to send back to PAYMENT_ADDR
ExCPU=1390000
ExMem=100
ExFct=2
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
  --testnet-magic 8
```

Failure to provide the correct --tx-in-datum-value results in MissingRequiredDatums
