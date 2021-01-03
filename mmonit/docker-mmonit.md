
## Build the M/Monit Image

```
# Build respective single arch image
./mmonit/scripts/build-docker.sh

MMONIT=3.7.6
VERSION="${MMONIT}-rev1"

docker manifest create nessusio/mmonit:${VERSION} \
  --amend nessusio/mmonit:${VERSION}-amd64 \
  --amend nessusio/mmonit:${VERSION}-arm64

docker manifest push nessusio/mmonit:${VERSION}

docker manifest create nessusio/mmonit:${MMONIT} \
  --amend nessusio/mmonit:${MMONIT}-amd64 \
  --amend nessusio/mmonit:${MMONIT}-arm64

docker manifest push nessusio/mmonit:${MMONIT}

docker manifest create nessusio/mmonit \
  --amend nessusio/mmonit:latest-amd64 \
  --amend nessusio/mmonit:latest-arm64

docker manifest push nessusio/mmonit
```

## Run M/Monit

Login: admin/swordfish

```
docker pull nessusio/mmonit

docker rm -f mmonit
docker run --detach \
  --name=mmonit \
  --restart=always \
  -p 8080:8080 \
  nessusio/mmonit -i

docker logs -f mmonit
```
