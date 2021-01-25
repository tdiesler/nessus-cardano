
## Run M/Monit

Login: admin/swordfish

```
docker pull nessusio/mmonit

VERSION=3.7.6-rev2

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
