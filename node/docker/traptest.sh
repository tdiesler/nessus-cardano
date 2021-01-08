#!/bin/bash

trap "echo SIGHUP" SIGHUP
trap "echo SIGINT" SIGINT
trap "echo SIGQUIT" SIGQUIT
trap "echo SIGKILL" SIGKILL
trap "echo SIGTERM; exit 0" SIGTERM

while true
do 
  echo "waiting ..."
  sleep 2
done
