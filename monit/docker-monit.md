
## Build the Monit Image

```
# Build respective single arch image
./monit/scripts/build-docker.sh

MONIT=5.27.1
VERSION="$MONIT-rev1"

docker manifest create nessusio/monit:$VERSION \
  --amend nessusio/monit:$VERSION-amd64 \
  --amend nessusio/monit:$VERSION-arm64

docker manifest push nessusio/monit:$VERSION

docker manifest create nessusio/monit:$MONIT \
  --amend nessusio/monit:$MONIT-amd64 \
  --amend nessusio/monit:$MONIT-arm64

docker manifest push nessusio/monit:$MONIT

docker manifest create nessusio/monit \
  --amend nessusio/monit:latest-amd64 \
  --amend nessusio/monit:latest-arm64

docker manifest push nessusio/monit
```

## Run Monit

### Setup the Config Volume

```
MMONIT_PORT=8080
MMONIT_ADDR=95.179.129.205
MMONIT_AUTH='username:changeit'

MONIT_AUTH=$MMONIT_AUTH

mkdir -p monit

cat << EOF > monit/monitrc-extras
set eventqueue basedir /var/monit/ slots 1000
set mmonit http://$MMONIT_AUTH@$MMONIT_ADDR:$MMONIT_PORT/collector
set httpd port 2812 and 
    use address 0.0.0.0    # bind to all interfaces (i.e. not just to localhost)
    allow $MMONIT_ADDR     # allow the M/Monit host to connect to the server
    allow $MONIT_AUTH      # monit authorization
EOF

docker rm -f monit
docker volume rm -f monit-config

docker run --name=tmp -v monit-config:/etc/monit.d/ centos
docker cp monit/monitrc-extras tmp:/etc/monit.d
docker run --rm -v monit-config:/etc/monit.d centos find /etc/monit.d -type f | sort
docker rm -f tmp
```

### Run the Image

```
docker rm -f monit
docker run --detach \
  --name=monit \
  -v monit-config:/etc/monit.d \
  nessusio/monit -Iv

docker logs -f monit
```
