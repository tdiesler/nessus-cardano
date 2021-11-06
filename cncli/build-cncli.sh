#!/usr/bin/env bash

set -x

docker build -t jterrier84/cncli \
  -f cncli.dockerfile . \
  2>&1 \
  | tee /tmp/build-cncli.logs
