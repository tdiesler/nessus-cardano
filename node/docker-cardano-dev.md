
## Run the Relay Node

```
# Create a private network
docker network create --subnet=172.18.0.0/16 cardano

# Run the Relay node

sudo rm -rf /mnt/disks/data00/*

docker rm -f relay
docker run --detach \
    --name=relay \
    -p 3001:3001 \
    --ip 172.18.0.10 \
    --network cardano \
    --hostname="relay" \
    -e CARDANO_UPDATE_TOPOLOGY=true \
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

