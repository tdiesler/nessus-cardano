
## Installing Google Cloud SDK

https://cloud.google.com/kubernetes-engine/docs/quickstart

https://cloud.google.com/sdk/docs/cheatsheet

To install gcloud and kubectl, perform the following steps:

  1) Install the Cloud SDK, which includes the gcloud command-line tool.
  2) Install the kubectl command-line tool

```
gcloud components install kubectl
```

## Configuring default settings

```
CLUSTER_NAME=cardano
CLUSTER_ZONE=us-central1-a

# Setting a default project ID
gcloud config set project beaconchain

# Setting a default compute zone or region
gcloud config set compute/zone $CLUSTER_ZONE
```

## Creating a GKE cluster

https://cloud.google.com/sdk/gcloud/reference/container/clusters/create

```
gcloud container clusters create $CLUSTER_NAME \
  --disk-size=32GB \
  --machine-type=e2-medium \
  --node-locations=$CLUSTER_ZONE \
  --zone=$CLUSTER_ZONE \
  --num-nodes=2
```

## Get credentials for the cluster

```
gcloud container clusters get-credentials $CLUSTER_NAME
```

## Delete the cluster

```
gcloud container clusters delete $CLUSTER_NAME \
  --zone=$CLUSTER_ZONE
```
