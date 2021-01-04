
## Run the Relay Node

```
docker network create --subnet=172.18.0.0/16 cardano

RELAY_PUBLIC_IP=`curl -s ipinfo.io/ip`
echo "RELAY_PUBLIC_IP: ${RELAY_PUBLIC_IP}"

# Setup the config volume
docker rm -f relay
docker volume rm -f cardano-relay-config
docker run --name=tmp -v cardano-relay-config:/var/cardano/config debian
docker cp ./node/docker/config/mainnet-config.json tmp:/var/cardano/config/mainnet-config.json
docker rm -f tmp

# Run the Relay node

sudo rm -rf /mnt/disks/data00/*

docker rm -f relay
docker run --detach \
    --name=relay \
    --restart=always \
    -p 3001:3001 \
    --ip 172.18.0.10 \
    --network cardano \
    --hostname="cdrelay" \
    -e CARDANO_CONFIG="/var/cardano/config/mainnet-config.json" \
    -v cardano-relay-config:/var/cardano/config  \
    -v /mnt/disks/data00:/opt/cardano/data \
    nessusio/cardano:dev run

docker logs -f relay

docker exec -it relay gLiveView
docker exec -it relay tail -f -n 100 /opt/cardano/logs/debug.log

# Access the EKG metric
docker exec -it relay curl -H 'Accept: application/json' 127.0.0.1:12788 | jq

# Access the Prometheus metrics
docker exec -it relay curl 127.0.0.1:12798/metrics | sort
```

