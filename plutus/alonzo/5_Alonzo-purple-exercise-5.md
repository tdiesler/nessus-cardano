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
cardano-cli address key-gen \
  --verification-key-file /var/cardano/local/keys/alonzo/wallets/shelley.vkey \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/shelley.skey \
&& cardano-cli address build \
  --payment-verification-key-file /var/cardano/local/keys/alonzo/wallets/shelley.vkey \
  --out-file /var/cardano/local/keys/alonzo/wallets/shelley.addr \
  --testnet-magic 8 \
&& cardano-cli address key-gen \
  --verification-key-file /var/cardano/local/keys/alonzo/wallets/mary.vkey \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/mary.skey \
&& cardano-cli address build \
  --payment-verification-key-file /var/cardano/local/keys/alonzo/wallets/mary.vkey \
  --out-file /var/cardano/local/keys/alonzo/wallets/mary.addr \
  --testnet-magic 8 \
&& cardano-cli address key-gen \
  --verification-key-file /var/cardano/local/keys/alonzo/wallets/percy.vkey \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/percy.skey \
&& cardano-cli address build \
  --payment-verification-key-file /var/cardano/local/keys/alonzo/wallets/percy.vkey \
  --out-file /var/cardano/local/keys/alonzo/wallets/percy.addr \
  --testnet-magic 8
```

Transfer some ada to each of these addresses, and check that they have been funded.

```
PAYMENT_ADDR0=$(cat ~/cardano/keys/alonzo/acc0/payment.addr)
WALLET_SHELLEY_ADDR=$(cat ~/cardano/keys/alonzo/wallets/shelley.addr)
WALLET_MARY_ADDR=$(cat ~/cardano/keys/alonzo/wallets/mary.addr)
WALLET_PERCY_ADDR=$(cat ~/cardano/keys/alonzo/wallets/percy.addr)

# Query UTxO
cardano-cli query utxo \
  --address $PAYMENT_ADDR0 \
  --testnet-magic 8

TX_IN1="d604bf2e1da2fb1b69db58be9e0e92c5ff8fb7d32133ab65f901d4fd350a00fc#0"

SEND_LVC=2000000000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-out $WALLET_SHELLEY_ADDR+$SEND_LVC \
  --tx-out $WALLET_SHELLEY_ADDR+$SEND_LVC \
  --tx-out $WALLET_SHELLEY_ADDR+$SEND_LVC \
  --tx-out $WALLET_MARY_ADDR+$SEND_LVC \
  --tx-out $WALLET_MARY_ADDR+$SEND_LVC \
  --tx-out $WALLET_MARY_ADDR+$SEND_LVC \
  --tx-out $WALLET_PERCY_ADDR+$SEND_LVC \
  --tx-out $WALLET_PERCY_ADDR+$SEND_LVC \
  --tx-out $WALLET_PERCY_ADDR+$SEND_LVC \
  --change-address $PAYMENT_ADDR0 \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/acc0/payment.skey \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8

# Query wallet UTxOs
cardano-cli query utxo \
  --address $WALLET_SHELLEY_ADDR \
  --testnet-magic 8 \
&& cardano-cli query utxo \
  --address $WALLET_MARY_ADDR \
  --testnet-magic 8 \
&& cardano-cli query utxo \
  --address $WALLET_PERCY_ADDR \
  --testnet-magic 8
```

### 5.2. Define a Mary-era minting script

Define a Mary-era _minting script_ (a "multi-signature" script) that allows _shelley_ to create new **Ozymandian** tokens. Define a _minting policy_ for the **Shelley** currency that uses this _minting script_. **Do not use Plutus scripts at this stage.**

https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/simple-scripts.md
https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/multi-assets.md

```
cardano-cli address key-gen \
  --verification-key-file /var/cardano/local/keys/alonzo/wallets/shelley-policy1.vkey \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/shelley-policy1.skey

POLICY_ONE_PKH=$(cardano-cli address key-hash --payment-verification-key-file /var/cardano/local/keys/alonzo/wallets/shelley-policy1.vkey) \
  && POLICY_ONE_PKH=${POLICY_ONE_PKH:0:56} \
  && echo "POLICY_ONE_PKH=\"$POLICY_ONE_PKH\""

cat << EOF > ~/cardano/keys/alonzo/wallets/shelley-policy1.script
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
POLICY_ID=$(cardano-cli transaction policyid \
  --script-file /var/cardano/local/keys/alonzo/wallets/shelley-policy1.script) \
  && POLICY_ID=${POLICY_ID:0:56} \
  && echo "POLICY_ID=\"$POLICY_ID\""

POLICY_ID="86cd90d1ebe6bb35931f4a8f9a69714721fad0bbfe01ebdf70816cd1"
```

### 5.3. Mint 1000 new Ozymandian in the percy address

Mint 1000 new **Ozymandian** in the _percy_ address by building and submitting a transaction. Check that they have been successfully minted.

```
cardano-cli query utxo \
  --address $WALLET_SHELLEY_ADDR \
  --testnet-magic 8

TX_IN1="8265b186c9013bb092d7c05b354dc538e095d9c072389e20e386661b6b2f8b3f#1"

SEND_AMOUNT=1500000

ASSET_NAME=Ozymandian
ASSET_AMOUNT=1000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-out "$WALLET_PERCY_ADDR+$SEND_AMOUNT+$ASSET_AMOUNT $POLICY_ID.$ASSET_NAME" \
  --mint "$ASSET_AMOUNT $POLICY_ID.$ASSET_NAME" \
  --mint-script-file /var/cardano/local/keys/alonzo/wallets/shelley-policy1.script \
  --witness-override 2 \
  --change-address $WALLET_SHELLEY_ADDR \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/shelley.skey \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/shelley-policy1.skey \
  --testnet-magic 8 \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

### 5.4 Define a second minting script

Define a second _minting script_ that allows _shelley_ to create new **SkyLark** tokens. Mint 100 **SkyLark** tokens and send them to _percy_. Check that the tokens have been received and then send 75 **SkyLark** tokens to _mary._

```
cardano-cli address key-gen \
  --verification-key-file /var/cardano/local/keys/alonzo/wallets/shelley-policy2.vkey \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/shelley-policy2.skey

POLICY_TWO_PKH=$(cardano-cli address key-hash --payment-verification-key-file /var/cardano/local/keys/alonzo/wallets/shelley-policy2.vkey) \
  && POLICY_TWO_PKH=${POLICY_TWO_PKH:0:56} \
  && echo "POLICY_TWO_PKH=\"$POLICY_TWO_PKH\""

cat << EOF > ~/cardano/keys/alonzo/wallets/shelley-policy2.script
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
POLICY_ID=$(cardano-cli transaction policyid \
  --script-file /var/cardano/local/keys/alonzo/wallets/shelley-policy2.script) \
  && POLICY_ID=${POLICY_ID:0:56} \
  && echo "POLICY_ID=\"$POLICY_ID\""

POLICY_ID="7a4c4257082168b746c10fc355850d8b4442321afd34af864c1b9984"
```

Mint 100 **SkyLark** tokens and send them to _percy_

```
cardano-cli query utxo \
  --address $WALLET_SHELLEY_ADDR \
  --testnet-magic 8

TX_IN1="132b80875f6da8a004c7fbf465a96afe7a7b9fdcf234b64de87eabbc383db11c#0"

SEND_AMOUNT=1500000

ASSET_NAME=SkyLark
ASSET_AMOUNT=100

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-out "$WALLET_PERCY_ADDR+$SEND_AMOUNT+$ASSET_AMOUNT $POLICY_ID.$ASSET_NAME" \
  --mint "$ASSET_AMOUNT $POLICY_ID.$ASSET_NAME" \
  --mint-script-file /var/cardano/local/keys/alonzo/wallets/shelley-policy2.script \
  --witness-override 2 \
  --change-address $WALLET_SHELLEY_ADDR \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/shelley.skey \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/shelley-policy2.skey \
  --testnet-magic 8 \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

send 75 **SkyLark** tokens from _percy_ to _mary_

```
# Query wallet UTxOs
cardano-cli query utxo \
  --address $WALLET_PERCY_ADDR \
  --testnet-magic 8 \
&& cardano-cli query utxo \
    --address $WALLET_MARY_ADDR \
    --testnet-magic 8

TX_IN1="b8d559d66b0bfed2f8efcf228fbbb7aaef597cf50b63d751adb70524fad9bfe4#1"
TX_IN2="bc8f72d1c323fbf24a846767ee4007b007e62f3ec7eb5a2b3a8e1d5faf324def#2"

SEND_AMOUNT=1500000

ASSET_NAME=SkyLark
ASSET_AMOUNT=75
TOKEN_REFUND=$((100 - $ASSET_AMOUNT))

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-in $TX_IN2 \
  --tx-out "$WALLET_MARY_ADDR+$SEND_AMOUNT+$ASSET_AMOUNT $POLICY_ID.$ASSET_NAME" \
  --tx-out "$WALLET_PERCY_ADDR+$SEND_AMOUNT+$TOKEN_REFUND $POLICY_ID.$ASSET_NAME" \
  --change-address $WALLET_PERCY_ADDR \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/percy.skey \
  --testnet-magic 8 \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

### 5.5 What is the least amount that you need to keep

What is the least amount of **Ada** that you need to keep in the _mary_ and _percy_ addresses? What is the least amount of **Ozymandian** or **SkyLarks** that you can keep in an address?

* A little over 1 ADA (minimum Tx value + a little for the token)
* One token unit

### 5.6 Burn some of your Ozymandian in the percy address

You want to _burn_ some of your **Ozymandian** in the _percy_ address. How do you do this? What happens to your **Ada** balances when you burn your tokens?

```
cardano-cli query utxo \
  --address $WALLET_PERCY_ADDR \
  --testnet-magic 8

TX_IN1="12bde5addc76127ea4f1531a18bf8b8c58af007fb68124c441f9b9eb339b814b#1"

SEND_AMOUNT=1500000

ASSET_NAME=Ozymandian
ASSET_AMOUNT=1000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-in "0ba918fa679af33237aa18415f82142a641395f43fb4a3087b9c75845a54708c#0" \
  --mint "-$ASSET_AMOUNT $POLICY_ID.$ASSET_NAME" \
  --mint-script-file /var/cardano/local/keys/alonzo/wallets/shelley-policy1.script \
  --witness-override 2 \
  --change-address $WALLET_PERCY_ADDR \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/percy.skey \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/shelley-policy1.skey \
  --testnet-magic 8 \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

### 5.7 Define a Plutus minting script

Define a Plutus _minting script_ that allows you to mint a variable number of **Ozymandian** and **SkyLark** tokens (with the numbers supplied via a redeemer).

```
cd ~/git/nessus-cardano/plutus/alonzo/plutus-sources/plutus-tokens \
  && cabal run plutus-tokens minttokens.plutus \
  && cp minttokens.plutus ../../plutus-scripts \
  && mv minttokens.plutus ~/cardano/scripts \
  && POLICY_ID=$(cardano-cli transaction policyid \
    --script-file /var/cardano/local/scripts/minttokens.plutus) \
  && POLICY_ID=${POLICY_ID:0:56} \
  && echo "POLICY_ID=\"$POLICY_ID\""

POLICY_ID="f91a4655297977e3f4314d2abe4b74b0be1027a4cb9bd4890efe679b"
```

Verify that this works as you expect.

```
cardano-cli query utxo \
  --address $WALLET_SHELLEY_ADDR \
  --testnet-magic 8

TX_IN1="072c432a2df6596b85269e7b5a6ccecdd777362532eb0452d28c7759cda4660f#0"

TX_COL="ed176363ec67f8cc0bedf3e7cfeee8db528c5ff412e42fdbe0237f9173a7dc55#3"

SEND_AMOUNT=1500000

ASSET_NAME=SkyLark
ASSET_AMOUNT=1000

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-in-collateral $TX_COL \
  --tx-out "$WALLET_SHELLEY_ADDR+$SEND_AMOUNT+$ASSET_AMOUNT $POLICY_ID.$ASSET_NAME" \
  --mint "$ASSET_AMOUNT $POLICY_ID.$ASSET_NAME" \
  --mint-script-file /var/cardano/local/scripts/minttokens.plutus \
  --mint-redeemer-value $ASSET_AMOUNT \
  --change-address $WALLET_SHELLEY_ADDR \
  --protocol-params-file /var/cardano/local/scratch/protocol.json \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/shelley.skey \
  --testnet-magic 8 \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

Burn those tokens again

```
cardano-cli query utxo \
  --address $WALLET_SHELLEY_ADDR \
  --testnet-magic 8

TX_IN1="371a4e11ea1848e8e6b0d6c29ee2aa7319ebddcd1d80f1eeddb58abcbdd52e2d#1"

TX_PAY="371a4e11ea1848e8e6b0d6c29ee2aa7319ebddcd1d80f1eeddb58abcbdd52e2d#0"

TX_COL="ed176363ec67f8cc0bedf3e7cfeee8db528c5ff412e42fdbe0237f9173a7dc55#3"

# Build, sign and submit the transaction
cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 8 \
  --tx-in $TX_IN1 \
  --tx-in $TX_PAY \
  --tx-in-collateral $TX_COL \
  --mint "-$ASSET_AMOUNT $POLICY_ID.$ASSET_NAME" \
  --mint-script-file /var/cardano/local/scripts/minttokens.plutus \
  --mint-redeemer-value -$ASSET_AMOUNT \
  --change-address $WALLET_SHELLEY_ADDR \
  --protocol-params-file /var/cardano/local/scratch/protocol.json \
  --out-file /var/cardano/local/scratch/tx.raw \
&& cardano-cli transaction sign \
  --tx-body-file /var/cardano/local/scratch/tx.raw \
  --signing-key-file /var/cardano/local/keys/alonzo/wallets/shelley.skey \
  --testnet-magic 8 \
  --out-file /var/cardano/local/scratch/tx.signed \
&& cardano-cli transaction submit \
  --tx-file /var/cardano/local/scratch/tx.signed \
  --testnet-magic 8
```

8. Define a Plutus _minting script_ that allows you to mint a single instance of a _non-fungible token_. Your script should take a payment from a user-supplied address and pass this payment to an address of your choice.

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
