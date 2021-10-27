#!/usr/bin/env bash

docker stop relay
docker logs -n100 relay 2> rts-stats.out
docker start relay
