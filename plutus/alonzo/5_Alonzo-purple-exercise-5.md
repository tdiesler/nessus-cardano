# Alonzo Purple Testnet Exercise Sheet 5: "Multi-Asset Tokens"

In the fourth exercise, you wrote some simple transactions using datums and redeemers. In this exercise, we will practice managing multi-asset tokens.

## Objectives ##

In the fifth set of exercises, we will make sure that you can:

1. Mint native tokens using both Mary-era and Plutus scripts
2. Redeem native tokens
3. Mint non-fungible tokens, including taking payments
4. Manage time-based scripts

## Exercises ##

### 5.1. Create a set of private/public signing keys

Create a set of private/public signing keys, _shelley_, and two _payment addresses, mary_ and _percy_. Fund the addresses with some test Ada.

```
SHELLEY_ADDR=$(cat ~/cardano/keys/testnet/acc1/payment.addr) \
&& MARY_ADDR=$(cat ~/cardano/keys/testnet/acc2/payment.addr) \
&& PERCY_ADDR=$(cat ~/cardano/keys/testnet/acc3/payment.addr)
```

Transfer some ada to each of these addresses, and check that they have been funded.

### 5.2. Define a Mary-era minting script

Define a Mary-era _minting script_ (a "multi-signature" script) that allows _shelley_ to create new **Ozymandian** tokens. Define a _minting policy_ for the **Shelley** currency that uses this _minting script_. **Do not use Plutus scripts at this stage.**

https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/simple-scripts.md
https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/multi-assets.md

```
cardano-cli address key-gen \
  --verification-key-file /var/cardano/local/keys/testnet/acc1/shelley-policy1.vkey \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/shelley-policy1.skey

POLICY_ONE_PKH=$(cardano-cli address key-hash --payment-verification-key-file /var/cardano/local/keys/testnet/acc1/shelley-policy1.vkey) \
  && POLICY_ONE_PKH=${POLICY_ONE_PKH:0:56} \
  && echo "POLICY_ONE_PKH=\"$POLICY_ONE_PKH\""

cat << EOF > ~/cardano/keys/testnet/acc1/shelley-policy1.script
{
  "type": "all",
  "scripts":
  [
    {
      "type": "sig",
      "keyHash": "$POLICY_ONE_PKH"
    }
  ]
}
EOF

# Create Policy Id
POLICY_ID1=$(cardano-cli transaction policyid \
  --script-file /var/cardano/local/keys/testnet/acc1/shelley-policy1.script) \
  && POLICY_ID1=${POLICY_ID1:0:56} \
  && echo "POLICY_ID1=\"$POLICY_ID1\""

POLICY_ID1="06dc75fd11b2587b53b3012238a95aaf6bdab4f0eb38530fb3fa8cba"
```

### 5.3. Mint 1000 new Ozymandian in the percy address

Mint 1000 new **Ozymandian** in the _percy_ address by building and submitting a transaction. Check that they have been successfully minted.

```
cardano-cli query utxo \
  --address $SHELLEY_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="e24826aa9927e856bbf8de5b24785d84164c4af1100f71c0a17212422e0ed962#0"

SEND_AMOUNT=1500000

ASSET_NAME=Ozymandian
ASSET_AMOUNT=1000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN1 \
  --tx-out "$PERCY_ADDR+$SEND_AMOUNT+$ASSET_AMOUNT $POLICY_ID1.$ASSET_NAME" \
  --mint "$ASSET_AMOUNT $POLICY_ID1.$ASSET_NAME" \
  --mint-script-file /var/cardano/local/keys/testnet/acc1/shelley-policy1.script \
  --witness-override 2 \
  --change-address $SHELLEY_ADDR \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/payment.skey \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/shelley-policy1.skey \
  --testnet-magic $TESTNET_MAGIC \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

### 5.4 Define a second minting script

Define a second _minting script_ that allows _shelley_ to create new **SkyLark** tokens. Mint 100 **SkyLark** tokens and send them to _percy_. Check that the tokens have been received and then send 75 **SkyLark** tokens to _mary._

```
cardano-cli address key-gen \
  --verification-key-file /var/cardano/local/keys/testnet/acc1/shelley-policy2.vkey \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/shelley-policy2.skey

POLICY_TWO_PKH=$(cardano-cli address key-hash --payment-verification-key-file /var/cardano/local/keys/testnet/acc1/shelley-policy2.vkey) \
  && POLICY_TWO_PKH=${POLICY_TWO_PKH:0:56} \
  && echo "POLICY_TWO_PKH=\"$POLICY_TWO_PKH\""

cat << EOF > ~/cardano/keys/testnet/acc1/shelley-policy2.script
{
  "type": "all",
  "scripts":
  [
    {
      "type": "sig",
      "keyHash": "$POLICY_TWO_PKH"
    }
  ]
}
EOF

# Create Policy Id
POLICY_ID2=$(cardano-cli transaction policyid \
  --script-file /var/cardano/local/keys/testnet/acc1/shelley-policy2.script) \
  && POLICY_ID2=${POLICY_ID2:0:56} \
  && echo "POLICY_ID2=\"$POLICY_ID2\""

POLICY_ID2="6a5996174da3738d61e27edf7c01b0efb1186794c9c5d9270a3d70a1"
```

Mint 100 **SkyLark** tokens and send them to _percy_

```
cardano-cli query utxo \
  --address $SHELLEY_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="18fc0c2fb7fd88d8719e606da7426b523cdadf52c5f56561f393605d209b86df#0"

SEND_AMOUNT=1500000

ASSET_NAME=SkyLark
ASSET_AMOUNT=100

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN1 \
  --tx-out "$PERCY_ADDR+$SEND_AMOUNT+$ASSET_AMOUNT $POLICY_ID2.$ASSET_NAME" \
  --mint "$ASSET_AMOUNT $POLICY_ID2.$ASSET_NAME" \
  --mint-script-file /var/cardano/local/keys/testnet/acc1/shelley-policy2.script \
  --witness-override 2 \
  --change-address $SHELLEY_ADDR \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/payment.skey \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/shelley-policy2.skey \
  --testnet-magic $TESTNET_MAGIC \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

send 75 **SkyLark** tokens from _percy_ to _mary_

```
# Query wallet UTxOs
cardano-cli query utxo \
  --address $PERCY_ADDR \
  --testnet-magic $TESTNET_MAGIC \
&& cardano-cli query utxo \
  --address $MARY_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="0741a09c5da17fc60f4e59c42a9d10f63d9d9a263be3e84266282615e83b60af#1"
TX_IN2="076f062a8d038709ff78943339b03196636366c45439b89135949163a9138232#1"

SEND_AMOUNT=1500000

ASSET_NAME=SkyLark
ASSET_AMOUNT=75
TOKEN_REFUND=$((100 - $ASSET_AMOUNT))

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN1 \
  --tx-in $TX_IN2 \
  --tx-out "$MARY_ADDR+$SEND_AMOUNT+$ASSET_AMOUNT $POLICY_ID2.$ASSET_NAME" \
  --tx-out "$PERCY_ADDR+$SEND_AMOUNT+$TOKEN_REFUND $POLICY_ID2.$ASSET_NAME" \
  --change-address $PERCY_ADDR \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc3/payment.skey \
  --testnet-magic $TESTNET_MAGIC \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

### 5.5 What is the least amount that you need to keep

What is the least amount of **Ada** that you need to keep in the _mary_ and _percy_ addresses? What is the least amount of **Ozymandian** or **SkyLarks** that you can keep in an address?

* A little over 1 ADA (minimum Tx value + a little for the token)
* One token unit

### 5.6 Burn some of your Ozymandian in the percy address

You want to _burn_ some of your **Ozymandian** in the _percy_ address. How do you do this? What happens to your **Ada** balances when you burn your tokens?

```
cardano-cli query utxo \
  --address $PERCY_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="18fc0c2fb7fd88d8719e606da7426b523cdadf52c5f56561f393605d209b86df#1"

ASSET_NAME=Ozymandian
ASSET_AMOUNT=1000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN1 \
  --mint "-$ASSET_AMOUNT $POLICY_ID1.$ASSET_NAME" \
  --mint-script-file /var/cardano/local/keys/testnet/acc1/shelley-policy1.script \
  --witness-override 2 \
  --change-address $PERCY_ADDR \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc3/payment.skey \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/shelley-policy1.skey \
  --testnet-magic $TESTNET_MAGIC \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

Burn all **SkyLark** tokens

```
cardano-cli query utxo \
  --address $MARY_ADDR \
  --testnet-magic $TESTNET_MAGIC \
&& cardano-cli query utxo \
  --address $PERCY_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="8684ed93bd55966181b239766c50337c1dc9e61b617488cbb1f3c0b6d54dccd3#1"
TX_IN2="a8ef2224643bff5e258de476f33899baaba66fe92f63deb78ff83f63fc4314fd#0"

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN1 \
  --tx-in $TX_IN2 \
  --mint "-75 $POLICY_ID2.SkyLark" \
  --mint-script-file /var/cardano/local/keys/testnet/acc1/shelley-policy2.script \
  --witness-override 2 \
  --change-address $MARY_ADDR \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc2/payment.skey \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/shelley-policy2.skey \
  --testnet-magic $TESTNET_MAGIC \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="539123f8eda1ffc0a4a9744406a32ab59000453c6685ea1c002126bfd3324102#0"
TX_IN2="8684ed93bd55966181b239766c50337c1dc9e61b617488cbb1f3c0b6d54dccd3#0"
TX_IN3="8684ed93bd55966181b239766c50337c1dc9e61b617488cbb1f3c0b6d54dccd3#2"

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN1 \
  --tx-in $TX_IN2 \
  --tx-in $TX_IN3 \
  --mint "-25 $POLICY_ID2.SkyLark" \
  --mint-script-file /var/cardano/local/keys/testnet/acc1/shelley-policy2.script \
  --witness-override 2 \
  --change-address $PERCY_ADDR \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc3/payment.skey \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/shelley-policy2.skey \
  --testnet-magic $TESTNET_MAGIC \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

### 5.7 Define a Plutus minting script

Define a Plutus _minting script_ that allows you to mint a variable number of **Ozymandian** and **SkyLark** tokens (with the numbers supplied via a redeemer).

```
cd ~/git/nessus-cardano/plutus/alonzo/plutus-sources/plutus-tokens \
  && cabal run plutus-tokens minttokens.plutus \
  && cp minttokens.plutus ../../plutus-scripts \
  && mv minttokens.plutus ~/cardano/scripts \
  && POLICY_ID3=$(cardano-cli transaction policyid \
    --script-file /var/cardano/local/scripts/minttokens.plutus) \
  && POLICY_ID3=${POLICY_ID3:0:56} \
  && echo "POLICY_ID3=\"$POLICY_ID3\""

ExBudget {exBudgetCPU = ExCPU 8842681, exBudgetMemory = ExMemory 29800}
POLICY_ID3="7d270d409c16a3177be53acaa9cb48800daff8194ce1f9d6350492ba"
```

Mint a variable number of **Ozymandian** and **SkyLark** tokens

```
cardano-cli query utxo \
  --address $SHELLEY_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="0741a09c5da17fc60f4e59c42a9d10f63d9d9a263be3e84266282615e83b60af#0"

TX_COL="689abea1c788f21f191aa2f05cf8367687d0dff3f514cc41a3af8e9f2a0d5d8b#4"

SEND_AMOUNT=1500000

ASSET_NAME=SkyLark
ASSET_AMOUNT=1000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN1 \
  --tx-in-collateral $TX_COL \
  --mint "$ASSET_AMOUNT $POLICY_ID3.$ASSET_NAME" \
  --mint-script-file /var/cardano/local/scripts/minttokens.plutus \
  --mint-redeemer-value $ASSET_AMOUNT \
  --tx-out "$SHELLEY_ADDR+$SEND_AMOUNT+$ASSET_AMOUNT $POLICY_ID3.$ASSET_NAME" \
  --change-address $SHELLEY_ADDR \
  --protocol-params-file /var/cardano/local/scratch/protocol.json \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/payment.skey \
  --testnet-magic $TESTNET_MAGIC \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

Burn those tokens again

```
cardano-cli query utxo \
  --address $SHELLEY_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="22beacdcd22da34341b64d722c559e9a789a143c1f4dfd67b22cf16c64a90a5e#0"
TX_IN2="22beacdcd22da34341b64d722c559e9a789a143c1f4dfd67b22cf16c64a90a5e#1"

TX_COL="689abea1c788f21f191aa2f05cf8367687d0dff3f514cc41a3af8e9f2a0d5d8b#4"

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN1 \
  --tx-in $TX_IN2 \
  --tx-in-collateral $TX_COL \
  --mint "-$ASSET_AMOUNT $POLICY_ID3.$ASSET_NAME" \
  --mint-script-file /var/cardano/local/scripts/minttokens.plutus \
  --mint-redeemer-value -$ASSET_AMOUNT \
  --change-address $SHELLEY_ADDR \
  --protocol-params-file /var/cardano/local/scratch/protocol.json \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/payment.skey \
  --testnet-magic $TESTNET_MAGIC \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

### 5.8. Define a script that allows you to mint an NFT

Define a Plutus _minting script_ that allows you to mint a single instance of a _non-fungible token_.

```
cd ~/git/nessus-cardano/plutus/alonzo/plutus-sources/plutus-tokens \
  && cabal run plutus-nft mintnft.plutus \
  && cp mintnft.plutus ../../plutus-scripts \
  && mv mintnft.plutus ~/cardano/scripts \
  && POLICY_ID4=$(cardano-cli transaction policyid \
    --script-file /var/cardano/local/scripts/mintnft.plutus) \
  && POLICY_ID4=${POLICY_ID4:0:56} \
  && echo "POLICY_ID4=\"$POLICY_ID4\""

ExBudget {exBudgetCPU = ExCPU 8396086, exBudgetMemory = ExMemory 28300}
POLICY_ID4="a7cba91b4e33bd0425e93ad82cefac605109e7a10bf4e5a54eac5f8a"
```

Mint a single instance of a non-fungible token
Your script should take a payment from a user-supplied address and pass this payment to an address of your choice.

```
cardano-cli query utxo \
  --address $SHELLEY_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="f0263ee376730150b5b964f59a7ec93b1cccd0aafdaa12b4bcd47bff65c68645#0"

TX_COL="689abea1c788f21f191aa2f05cf8367687d0dff3f514cc41a3af8e9f2a0d5d8b#4"

SEND_AMOUNT=1500000

ASSET_NAME=SkyLark
ASSET_AMOUNT=1

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN1 \
  --tx-in-collateral $TX_COL \
  --mint "$ASSET_AMOUNT $POLICY_ID4.$ASSET_NAME" \
  --mint-script-file /var/cardano/local/scripts/mintnft.plutus \
  --mint-redeemer-value $ASSET_AMOUNT \
  --tx-out "$SHELLEY_ADDR+$SEND_AMOUNT+$ASSET_AMOUNT $POLICY_ID4.$ASSET_NAME" \
  --change-address $SHELLEY_ADDR \
  --protocol-params-file /var/cardano/local/scratch/protocol.json \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/payment.skey \
  --testnet-magic $TESTNET_MAGIC \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

Send that NFT to a trash can

```
# Create a trash can
cardano-cli address key-gen \
  --verification-key-file /var/cardano/local/scratch/trash.vkey \
  --signing-key-file /var/cardano/local/scratch/trash.skey \
&& cardano-cli address build \
  --payment-verification-key-file /var/cardano/local/scratch/trash.vkey \
  --out-file /var/cardano/local/scratch/trash.addr \
  --testnet-magic $TESTNET_MAGIC \
&& TRASH_ADDR=$(cat ~/cardano/scratch/trash.addr) \
&& echo "TRASH_ADDR=$TRASH_ADDR"

cardano-cli query utxo \
  --address $SHELLEY_ADDR \
  --testnet-magic $TESTNET_MAGIC

TX_IN1="78de187290c8703d14e0179daea824adce3dc4c32b0c21b69ef5c6b000914da3#0"
TX_IN2="78de187290c8703d14e0179daea824adce3dc4c32b0c21b69ef5c6b000914da3#1"

TX_COL="689abea1c788f21f191aa2f05cf8367687d0dff3f514cc41a3af8e9f2a0d5d8b#4"

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in $TX_IN1 \
  --tx-in $TX_IN2 \
  --tx-out "$TRASH_ADDR+$SEND_AMOUNT+$ASSET_AMOUNT $POLICY_ID4.$ASSET_NAME" \
  --change-address $SHELLEY_ADDR \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/testnet/acc1/payment.skey \
  --testnet-magic $TESTNET_MAGIC \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic $TESTNET_MAGIC
```

9. Adapt your solution from Exercise 8 so that you conduct a Dutch auction on your _non-fungible token._ For example, start the bidding at 1000 Ada and reduce the price by 1 Ada every second. Sell the non-fungible token to the first client that offers to pay at least the current price. When the price falls below your hidden _reserve_, reject all future bids.

10. Adapt your solution from Exercise 9 so that the auction for the non-fungible token starts at a predetermined time. Reject all bids that arrive before the time.

11. **Optional Exercise (Easy to Moderate)**

	Publicise your _non-fungible token sale_ and participate in other token sales. Aim to collect the most interesting set of non-fungible tokens. When selling your tokens, you may want to record some metadata on the chain (e.g. representing a digital image or the purchaser&#39;s identity) as well as transferring the non-fungible token itself. How can you do this?

12. **Optional Exercise (Moderate)**

	Implement a token "factory".  You should accept the token, the _minting policy_ and the required number of tokens as parameters to your script and mint the required number of tokens.  Does your solution work for both *fungible* and *non-fungible* tokens?  How do you deal with third-party signatories?  Test your solution by allowing another testnet user to mint new tokens using your factory.

## Feedback


**Please let us know of any problems that you have encountered**

- Via the Discord channels for general questions.

- Via the issue tracker at [https://github.com/input-output-hk/cardano-node/issues](https://github.com/input-output-hk/cardano-node/issues) for any bugs in the node etc.  Please tag them as Alonzo-related.

- Via the issue tracker at [https://github.com/input-output-hk/plutus/issues](https://github.com/input-output-hk/plutus/issues) for any bugs or feature requests with plutus, playground, PAB etc.

- Via the issue tracker at [https://github.com/input-output-hk/Alonzo-testnet/issues](https://github.com/input-output-hk/Alonzo-testnet/issues) for any issues with the exercises.
