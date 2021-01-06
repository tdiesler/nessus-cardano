
## Create a Transaction

```
OWNER=spo
rm -rf cardano/scratch
mkdir -p cardano/scratch

# Get protocol parameters
cardano-cli query protocol-parameters \
    --out-file /var/cardano/local/scratch/protocol.json \
    --allegra-era \
    --mainnet

# Get the transaction hash and index of the UTXO to spend

STAKE_ADDR=`cat cardano/keys/$OWNER/stake.addr`
PAYMENT_ADDR=`cat cardano/keys/pool/payment.addr`
echo "${STAKE_ADDR} => ${PAYMENT_ADDR}"

cardano-cli query stake-address-info \
    --mainnet \
    --allegra-era \
    --address ${STAKE_ADDR}

[
    {
        "address": "stake1u8z0dkadelc7sfqqzxndtuya67memkvnp3z3lvcgr4ak0xsnu63xr",
        "delegation": "pool1n6qqnvjfzskcq9zdld5pnp8q3ktd28pqsh5tkmvarqcayr76puv",
        "rewardAccountBalance": 494768141
    }
]

# Query the payment address balance

cardano-cli query utxo \
    --address ${PAYMENT_ADDR} \
    --allegra-era \
    --mainnet  
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
308a86b761afa16fcf1373fccc5e35fe7e797c75107b674d5d9456f16dfc127e     0        6371515 lovelace

# Draft the withdraw transaction to transfer the rewards to a payment.addr

TX_IN1="308a86b761afa16fcf1373fccc5e35fe7e797c75107b674d5d9456f16dfc127e#0"

cardano-cli transaction build-raw \
    --tx-in ${TX_IN1} \
    --tx-out ${PAYMENT_ADDR}+6371515 \
    --withdrawal ${STAKE_ADDR}+494768141 \
    --ttl 0 \
    --fee 0 \
    --out-file /var/cardano/local/scratch/tx.draft
    
# Calculate the fee  
  
cardano-cli transaction calculate-min-fee \
    --protocol-params-file /var/cardano/local/scratch/protocol.json \
    --tx-body-file /var/cardano/local/scratch/tx.draft \
    --tx-in-count 1 \
    --tx-out-count 1 \
    --witness-count 1 \
    --mainnet
    
> 171485 Lovelace

# Calculate the change to send back to PAYMENT_ADDR

WITHDRAW_LVC=494768141
UTOX_LVC=6371515
FEES_LVC=171485
SEND_LVC=`expr $UTOX_LVC + $WITHDRAW_LVC - $FEES_LVC`
echo "$SEND_LVC Lovelace"

# Determine the TTL (time to Live) for the transaction

cardano-cli query tip --mainnet
{
    ...
    "slotNo": 18343208
}
TTL=`expr 18343208 + 3600`
echo $TTL

# Build the transaction
cardano-cli transaction build-raw \
    --tx-in ${TX_IN1} \
    --tx-out ${PAYMENT_ADDR}+${SEND_LVC} \
    --withdrawal ${STAKE_ADDR}+${WITHDRAW_LVC} \
    --ttl $TTL \
    --fee $FEES_LVC \
    --out-file /var/cardano/local/scratch/tx.raw
    
# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file /var/cardano/local/scratch/tx.raw \
    --signing-key-file /var/cardano/local/keys/pool/payment.skey \
    --signing-key-file /var/cardano/local/keys/${OWNER}/stake.skey \
    --mainnet \
    --out-file /var/cardano/local/scratch/tx.signed

# Submit the transaction
cardano-cli transaction submit \
    --tx-file /var/cardano/local/scratch/tx.signed \
    --mainnet
```
