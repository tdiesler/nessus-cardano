
## Prepare Debian10 (Buster)

```
cat << EOF >> $HOME/.profile

# Locale and Language
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=C
EOF
source .profile

sudo apt update
sudo apt upgrade -y

sudo apt-get install -y git
```

## Install Cardano Bare Metal

```
git clone -b bare https://github.com/tdiesler/nessus-cardano.git
cd nessus-cardano
chmod +x images/node/scripts/build-bare-debian.sh
chmod +x images/node/scripts/setup-pool-config.sh

./images/node/scripts/build-bare-debian.sh

./images/node/scripts/setup-pool-config.sh
```
