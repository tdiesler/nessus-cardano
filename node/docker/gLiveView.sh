#!/bin/bash

signalHandler () {
  PID=`pgrep gLiveView.sh`
  echo "Terminating [$PID] ..."
  kill -SIGKILL $PID
}

# Trap the SIGHUP signal
trap "signalHandler" SIGHUP SIGTERM

# In case the above trap did not work kill all gLiveView processes
for i in `pgrep gLiveView`; do
  if [ "$$" != "$i" ]; then
    echo "Terminating [$i] ..."
    kill -SIGKILL $i
  fi
done

/root/guild-operators/scripts/cnode-helper-scripts/gLiveView.sh
