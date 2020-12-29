
## Build the Image

```
# Build respective single arch image
./images/node/scripts/build-docker.sh

CARDANO=1.24.2
VERSION="$CARDANO-rev1"

docker manifest create nessusio/cardano:$VERSION \
  --amend nessusio/cardano:$VERSION-amd64 \
  --amend nessusio/cardano:$VERSION-arm64

docker manifest push nessusio/cardano:$VERSION

docker manifest create nessusio/cardano:$CARDANO \
  --amend nessusio/cardano:$CARDANO-amd64 \
  --amend nessusio/cardano:$CARDANO-arm64

docker manifest push nessusio/cardano:$CARDANO

docker manifest create nessusio/cardano \
  --amend nessusio/cardano:latest-amd64 \
  --amend nessusio/cardano:latest-arm64

docker manifest push nessusio/cardano
```
