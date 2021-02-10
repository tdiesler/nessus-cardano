## Install Docker

* Ubuntu-20.04

### Setup the core user

```
ssh root@vps

sed -i "s/#PasswordAuthentication yes$/PasswordAuthentication no/" /etc/ssh/sshd_config
cat /etc/ssh/sshd_config | grep PasswordAuthentication
systemctl restart sshd

# Update the system
apt-get update
apt-get full-upgrade -y

# Install Time Service
timedatectl

export NUSER=core
useradd -G root -m $NUSER -s /bin/bash
cp -r .ssh /home/$NUSER/
chown -R $NUSER.$NUSER /home/$NUSER/.ssh

cat << EOF > /etc/sudoers.d/user-privs-$NUSER
$NUSER ALL=(ALL:ALL) NOPASSWD: ALL
EOF

# Start the SSH agent
eval $(ssh-agent)
```

### Swap setup

```
sudo fallocate -l 4G /mnt/swapfile
sudo dd if=/dev/zero of=/mnt/swapfile bs=1024 count=4M
sudo mkswap /mnt/swapfile
sudo chmod 600 /mnt/swapfile
sudo swapon /mnt/swapfile
echo '/mnt/swapfile none swap sw 0 0' | sudo tee /etc/fstab
free -h
```

### Install Nix

https://nixos.org/manual/nix/stable/#chap-installation

```
# Single user install
sh <(curl -L https://nixos.org/nix/install)
source ~/.nix-profile/etc/profile.d/nix.sh

# Upgrade Nix
nix-channel --update; nix-env -iA nixpkgs.nix
nix-collect-garbage -d
```

### Install Docker

https://docs.docker.com/engine/install/debian/

```
sudo apt-get install -y \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg-agent \
    software-properties-common

curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -

sudo add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   $(lsb_release -cs) \
   stable"

sudo apt-get update
sudo apt-get install -y docker-ce docker-ce-cli containerd.io

sudo systemctl enable docker
sudo systemctl start docker
sudo systemctl status docker

sudo usermod -aG docker ${USER}

# Verify docker access after restart
docker run --rm centos echo "Hello World"
```
