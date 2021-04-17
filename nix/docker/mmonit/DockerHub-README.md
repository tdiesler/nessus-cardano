
The Nessus M/Monit images are produced by the [Nessus Cardano](https://github.com/tdiesler/nessus-cardano) project.

These images generally follow [M/Monit](https://www.mmonit.com/download/) releases.

There may be multiple revisions of this image based on the same M/Monit release. For example ...

|              |                                                                                                                           |
|:-------------|:--------------------------------------------------------------------------------------------------------------------------|
| 3.7.6-rev1   | Denotes the first revision for M/Monit 3.7.6 |
| 3.7.6        | Is the latest revision for M/Monit 3.7.6 |
| latest       | Is the latest revision for the latest M/Monit release |

Each image comes in multiple arch variant. Current we support `amd64` and `arm64`.

## Running M/Monit

Login: admin/swordfish

```
VERSION=3.7.7-rev2

CONFDIR="/usr/local/var/mmonit/conf"
LICENSE="${CONFDIR}/license.xml"

docker rm -f mmonit
docker run --detach \
  --name=mmonit \
  -p 8080:8080 \
  --restart=always \
  -v ~/mmonit/conf/license.xml:${LICENSE} \
  nessusio/mmonit:${VERSION} -i

docker exec -it mmonit cat ${LICENSE}

docker logs -f mmonit
```
