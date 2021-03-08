
## Create an Instance

```
HOSTNAME=nix01

MACHINE_TYPE=e2-standard-2
DISK_SIZE=80GB

gcloud beta compute instances create $HOSTNAME \
  --zone=us-central1-a \
  --address=34.68.137.181 \
  --machine-type=$MACHINE_TYPE \
  --image-project=centos-cloud \
  --image=centos-8-v20210217 \
  --boot-disk-size=$DISK_SIZE
```

## Initialize

```
# Update the system
sudo yum update -y \
  && sudo yum install -y git jq

# Install Nix
sh <(curl -L https://nixos.org/nix/install) \
  && source ~/.nix-profile/etc/profile.d/nix.sh

# Configure Nix to use the binary cache from IOHK
sudo mkdir /etc/nix
cat << EOF | sudo tee /etc/nix/nix.conf
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
substituters = https://hydra.iohk.io https://cache.nixos.org
EOF

# Install Docker
sudo yum install -y yum-utils \
  && sudo yum-config-manager --add-repo https://download.docker.com/linux/centos/docker-ce.repo \
  && sudo yum install -y --allowerasing docker-ce docker-ce-cli containerd.io \
  && sudo usermod -aG docker $USER \
  && sudo systemctl enable --now docker

# Install Docker Compose
sudo curl -L "https://github.com/docker/compose/releases/download/1.28.5/docker-compose-Linux-x86_64" -o /usr/local/bin/docker-compose \
  && sudo chmod +x /usr/local/bin/docker-compose

# Initialize bare git repositories
mkdir -p ~/gitsrv/cardano-node ~/gitsrv/nessus-cardano \
  && git init --bare ~/gitsrv/cardano-node \
  && git init --bare ~/gitsrv/nessus-cardano \
  && git config --global pull.ff only
```

## Populate the Data Volume

```
EPOCH=251

mkdir ~/data \
  && scp core@relay01.astorpool.net:shelley-data-e$EPOCH.tgz . \
  && tar -xzv -C ~/data -f ~/shelley-data-e$EPOCH.tgz

docker run --name=tmp -v shelley-data:/data centos \
  && docker cp ~/data/protocolMagicId tmp:/data \
  && docker cp ~/data/immutable tmp:/data \
  && docker cp ~/data/ledger tmp:/data \
  && docker rm tmp
```
