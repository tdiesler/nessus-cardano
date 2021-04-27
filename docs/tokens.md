
## Create Adresses

```
VERSION=dev

NWMAGIC=1097911063

OWNER=testA

mkdir -p ~/cardano/keys/$OWNER \
  && cardano-cli address key-gen \
    --verification-key-file /var/cardano/local/keys/$OWNER/payment.vkey \
    --signing-key-file /var/cardano/local/keys/$OWNER/payment.skey \
  && cardano-cli address build \
    --payment-verification-key-file /var/cardano/local/keys/$OWNER/payment.vkey \
    --out-file /var/cardano/local/keys/$OWNER/payment.addr \
    --testnet-magic $NWMAGIC

PAYMENT_ADDR=`cat ~/cardano/keys/$OWNER/payment.addr` \
  && echo "$OWNER PAYMENT_ADDR=$PAYMENT_ADDR"
```

## Get Balances

```
# Query UTOX
for owner in testA testB testC; do
  PAYMENT_ADDR=`cat ~/cardano/keys/$owner/payment.addr`
  echo; echo "$owner: $PAYMENT_ADDR"
  cardano-cli query utxo \
    --address $PAYMENT_ADDR \
    --testnet-magic $NWMAGIC
done
```

## Create Policy Script

https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/simple-scripts.md
https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/multi-assets.md

```
# We run all what follows inside the container
# because for some reason we can't pass whitespace params through the docker run alias

docker run -it --rm \
  -v ~/cardano:/var/cardano/local \
  -v node-ipc:/opt/cardano/ipc \
  --entrypoint=bash \
  nessusio/cardano-node:$VERSION

cardano-cli address key-gen \
  --verification-key-file /var/cardano/local/keys/$OWNER/policy.vkey \
  --signing-key-file /var/cardano/local/keys/$OWNER/policy.skey

POLICY_HASH=$(cardano-cli address key-hash --payment-verification-key-file /var/cardano/local/keys/$OWNER/policy.vkey) \
  && echo "POLICY_HASH: $POLICY_HASH"

cat << EOF > /var/cardano/local/keys/$OWNER/policy.script
{
  "type": "all",
  "scripts":
  [
    {
      "type": "sig",
      "keyHash": "$POLICY_HASH"
    }
  ]
}
EOF

# Create Policy Id
cardano-cli transaction policyid \
  --script-file /var/cardano/local/keys/$OWNER/policy.script | tee /var/cardano/local/keys/$OWNER/policy.id

POLICYID=`cat /var/cardano/local/keys/$OWNER/policy.id` \
  && echo "POLICYID: $POLICYID"
```

## Mint Tokens

```
cardano-cli query protocol-parameters \
    --out-file /var/cardano/local/scratch/protocol.json \
    --testnet-magic $NWMAGIC

PAYMENT_ADDR=`cat /var/cardano/local/keys/$OWNER/payment.addr` \
  && echo "$OWNER PAYMENT_ADDR: $PAYMENT_ADDR"

cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic $NWMAGIC

  TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
44cbad9ad5d956fda29d7fb19a9831bd2142c162494d89e3215ef03c54652d1a     0        1000000000 lovelace

TX_IN1=44cbad9ad5d956fda29d7fb19a9831bd2142c162494d89e3215ef03c54652d1a#0

UTXO_LVC=1000000000

POLICYID=`cat /var/cardano/local/keys/$OWNER/policy.id` \
  && echo "POLICYID: $POLICYID"

ASSET_NAME=AstorE260
ASSET_AMOUNT=500

# Calculate the fee  
cardano-cli transaction build-raw \
    --tx-in $TX_IN1 \
    --tx-out "$PAYMENT_ADDR+0+$ASSET_AMOUNT $POLICYID.$ASSET_NAME" \
    --mint "$ASSET_AMOUNT $POLICYID.$ASSET_NAME" \
    --fee 0 \
    --out-file /var/cardano/local/scratch/tx.draft

cardano-cli transaction calculate-min-fee \
    --protocol-params-file /var/cardano/local/scratch/protocol.json \
    --tx-body-file /var/cardano/local/scratch/tx.draft \
    --tx-in-count 1 \
    --tx-out-count 1 \
    --witness-count 2 \
    --testnet-magic $NWMAGIC

> 180285 Lovelace

# Calculate the change to send back to PAYMENT_ADDR

FEES_LVC=180285
REFUND_LVC=`expr $UTXO_LVC - $FEES_LVC`
echo "$REFUND_LVC Lovelace"

SLOT=`cardano-cli query tip --testnet-magic $NWMAGIC | jq -c | jq ".slot"` \
  && TTL=`expr $SLOT + 3600` \
  && echo -e "Slot: $SLOT\nTTL:  $TTL"

# Build the transaction
cardano-cli transaction build-raw \
  --tx-in $TX_IN1 \
  --tx-out "$PAYMENT_ADDR+$REFUND_LVC+$ASSET_AMOUNT $POLICYID.$ASSET_NAME" \
  --mint "$ASSET_AMOUNT $POLICYID.$ASSET_NAME" \
  --ttl $TTL \
  --fee $FEES_LVC \
  --out-file /var/cardano/local/scratch/tx.raw

# Sign the transaction
cardano-cli transaction sign \
    --tx-body-file /var/cardano/local/scratch/tx.raw \
    --signing-key-file /var/cardano/local/keys/$OWNER/payment.skey \
    --signing-key-file /var/cardano/local/keys/$OWNER/policy.skey \
    --script-file /var/cardano/local/keys/$OWNER/policy.script \
    --testnet-magic $NWMAGIC \
    --out-file /var/cardano/local/scratch/tx.signed

# Submit the transaction
cardano-cli transaction submit \
    --tx-file /var/cardano/local/scratch/tx.signed \
    --testnet-magic $NWMAGIC
```

## Check Token Balance

```
cardano-cli query utxo \
  --address $PAYMENT_ADDR \
  --testnet-magic $NWMAGIC

  TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
55ce20fc5f25fb43a554b126f9dd9259ec8982531c71c5c840e246c665cb771a     0        999819715 lovelace + 500 e0e836304637ba4196c3671c4c9e841b2603db6f533c89559bfa5507.AstorE260
```
