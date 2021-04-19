
The Nessus Monit images are produced by the [Nessus Cardano](https://github.com/tdiesler/nessus-cardano) project.

These images generally follow [Monit](https://mmonit.com/monit) releases.

There may be multiple revisions of this image based on the same Monit release. For example ...

|              |                                                                                                                           |
|:-------------|:--------------------------------------------------------------------------------------------------------------------------|
| 5.27.1-rev1  | Denotes the first revision for Monit 5.27.1 |
| 5.27.1       | Is the latest revision for Monit 5.27.1 |
| latest       | Is the latest revision for the latest Monit release |

Each image comes in multiple arch variant. Current we support `amd64` and `arm64`.

## Running Monit

### Setup the Config Volume

```
MMONIT_PORT=8080
MMONIT_ADDR=108.61.165.223
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
VERSION=5.28.0-rev2

docker rm -f monit
docker run --detach \
  --name=monit \
  --hostname=ada01rl \
  --network=cardano \
  -v monit-config:/etc/monit.d \
  nessusio/monit:${VERSION} -Iv

docker logs -f monit
```
