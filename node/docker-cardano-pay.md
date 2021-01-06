
## Create a Transaction

```
alias cardano-cli="docker exec -it relay cardano-cli"

OWNER=pool
rm -rf cardano/scratch
mkdir -p cardano/scratch

# Get protocol parameters
cardano-cli query protocol-parameters \
    --out-file /var/cardano/local/scratch/protocol.json \
    --allegra-era \
    --mainnet

# Get the transaction hash and index of the UTXO to spend

PAYMENT_ADDR=`cat cardano/keys/$OWNER/payment.addr`
echo $PAYMENT_ADDR

cardano-cli query utxo \
    --address $PAYMENT_ADDR \
    --allegra-era \
    --mainnet  
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8719beaa0af823eb902ccc882363eea2575269a87dbbe7c67083f573af381268     0        500968171 lovelace

# Draft the transaction

TX_IN1="8719beaa0af823eb902ccc882363eea2575269a87dbbe7c67083f573af381268#0"

TO_ADDR="addr1xxx"
REFUND_ADDR="$PAYMENT_ADDR"

cardano-cli transaction build-raw \
    --tx-in $TX_IN1 \
    --tx-out $TO_ADDR+495000000 \
    --tx-out $REFUND_ADDR+0 \
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
    
> 172497 Lovelace

# Calculate the change to send back to PAYMENT_ADDR

UTOX_LVC=500968171
SEND_LVC=495000000
FEES_LVC=172497
REFUND_LVC=`expr $UTOX_LVC - $SEND_LVC - $FEES_LVC`
echo "$REFUND_LVC Lovelace"

# Determine the TTL (time to Live) for the transaction

cardano-cli query tip --mainnet
{
    ...
    "slotNo": 18345050
}
TTL=`expr 18345050 + 3600`
echo $TTL

# Build the transaction
cardano-cli transaction build-raw \
    --tx-in $TX_IN1 \
    --tx-out $TO_ADDR+$SEND_LVC \
    --tx-out $REFUND_ADDR+$REFUND_LVC \
    --ttl $TTL \
    --fee $FEES_LVC \
    --out-file /var/cardano/local/scratch/tx.raw
    
# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file /var/cardano/local/scratch/tx.raw \
    --signing-key-file /var/cardano/local/keys/$OWNER/payment.skey \
    --mainnet \
    --out-file /var/cardano/local/scratch/tx.signed

# Submit the transaction
cardano-cli transaction submit \
    --tx-file /var/cardano/local/scratch/tx.signed \
    --mainnet
```
    