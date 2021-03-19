
## Creating a GKE cluster

https://cloud.google.com/sdk/gcloud/reference/container/clusters/create

```
CLUSTER_NAME=cardano
CLUSTER_ZONE=us-central1-a

gcloud container clusters create $CLUSTER_NAME \
  --node-labels=node-type=block-producer \
  --machine-type=e2-medium \
  --node-locations=$CLUSTER_ZONE \
  --zone=$CLUSTER_ZONE \
  --disk-size=20GB \
  --num-nodes=1

gcloud container node-pools create relay-pool \
  --node-labels=node-type=relay \
  --cluster=$CLUSTER_NAME \
  --machine-type=e2-medium \
  --node-locations=$CLUSTER_ZONE \
  --zone=$CLUSTER_ZONE \
  --tags=cardano-relay \
  --disk-size=20GB \
  --enable-autoscaling \
  --max-nodes=10 \
  --num-nodes=1
```

## Cardano Namespace

```
kubectl create namespace cardano

kubectl config set-context --current --namespace=cardano
```

## Create a Firewall Rule

```
gcloud compute firewall-rules create cardano-relay --allow tcp:$NODE_PORT
```

## Deploy a StatefulSet

```
kubectl apply -f nix/docker/k8s/cardano-stafulset.yaml

kubectl get sts,pod,svc

kubectl exec -it relay-0 -- cat /var/cardano/config/mainnet-topology.json
kubectl exec -it block-producer-0 -- cat /var/cardano/config/mainnet-topology.json

kubectl logs --tail=200 -f relay-0
kubectl logs --tail=200 -f block-producer-0

kubectl exec -it relay-0 -- gLiveView
kubectl exec -it block-producer-0 -- gLiveView

kubectl delete sts --all
```

## Scaling the Relay set

```
kubectl scale statefulsets relay --replicas=4

for i in {0..4}; do
  if [[ $i == 0 ]]; then
    pod="pod/block-producer-0"
  else
    pod="pod/relay-$(($i-1))"
  fi
  nodeName=$(kubectl get $pod -o json | jq -r .spec.nodeName)
  echo "$pod => $nodeName"
done
```

## Delete the cluster

```
gcloud container clusters delete $CLUSTER_NAME \
  --zone=$CLUSTER_ZONE
```

## Delete the data disks

```
for d in $(gcloud compute disks list --zones=$CLUSTER_ZONE --format="value(name)"); do
  gcloud compute disks delete $d --quiet
done
```
