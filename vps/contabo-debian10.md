
* Debian 10

### Setup the core user

```
ssh root@vps

# Update the system
apt-get update \
  && apt-get full-upgrade -y
  && apt-get install -y sudo curl

# Check Time Service
timedatectl

NUSER=core
useradd -G root -m $NUSER -s /bin/bash
mkdir /home/$NUSER/.ssh
echo "ssh-rsa AAAA..." > /home/$NUSER/.ssh/authorized_keys
chmod 700 /home/$NUSER/.ssh
chown -R $NUSER.$NUSER /home/$NUSER/.ssh

cat << EOF > /etc/sudoers.d/user-privs-$NUSER
$NUSER ALL=(ALL:ALL) NOPASSWD: ALL
EOF

# Assign a random SSH port above 10000
rnd=$RANDOM; echo $rnd
while (($rnd <= 10000)); do rnd=$(($rnd + $RANDOM)); echo $rnd; done
sed -i "s/#Port 22$/Port $rnd/" /etc/ssh/sshd_config

# Disable password authentication
sed -i "s/PasswordAuthentication yes$/PasswordAuthentication no/" /etc/ssh/sshd_config

# Disable password authentication
sed -i "s/ChallengeResponseAuthentication yes$/ChallengeResponseAuthentication no/" /etc/ssh/sshd_config

# Disable root login
sed -i "s/PermitRootLogin yes$/PermitRootLogin no/" /etc/ssh/sshd_config

# Disable X11Forwarding
sed -i "s/X11Forwarding yes$/X11Forwarding no/" /etc/ssh/sshd_config

cat /etc/ssh/sshd_config | egrep "^Port"
cat /etc/ssh/sshd_config | egrep "^PasswordAuthentication"
cat /etc/ssh/sshd_config | egrep "^ChallengeResponseAuthentication"
cat /etc/ssh/sshd_config | egrep "^PermitRootLogin"
cat /etc/ssh/sshd_config | egrep "^X11Forwarding"

systemctl restart sshd

# ------------------------------------------------------------------------------
# SSH login to core@xxx.xxx.xxx.xxx from some other terminal
# ------------------------------------------------------------------------------

# Assign a random SSH port above 10000
rnd=$RANDOM; echo $rnd
while (($rnd <= 10000)); do rnd=$(($rnd + $RANDOM)); echo $rnd; done
sed -i "s/#Port 22$/Port $rnd/" /etc/ssh/sshd_config

# Disable password authentication
sed -i "s/PasswordAuthentication yes$/PasswordAuthentication no/" /etc/ssh/sshd_config

# Disable password authentication
sed -i "s/ChallengeResponseAuthentication yes$/ChallengeResponseAuthentication no/" /etc/ssh/sshd_config

# Disable root login
sed -i "s/PermitRootLogin yes$/PermitRootLogin no/" /etc/ssh/sshd_config

# Disable X11Forwarding
sed -i "s/X11Forwarding yes$/X11Forwarding no/" /etc/ssh/sshd_config

cat /etc/ssh/sshd_config | egrep "^Port"
cat /etc/ssh/sshd_config | egrep "^PasswordAuthentication"
cat /etc/ssh/sshd_config | egrep "^ChallengeResponseAuthentication"
cat /etc/ssh/sshd_config | egrep "^PermitRootLogin"
cat /etc/ssh/sshd_config | egrep "^X11Forwarding"

systemctl restart sshd

# Start the SSH agent
# eval $(ssh-agent)
```

### Swap setup

```
sudo fallocate -l 8G /mnt/swapfile
sudo dd if=/dev/zero of=/mnt/swapfile bs=1024 count=8M
sudo mkswap /mnt/swapfile
sudo chmod 600 /mnt/swapfile
sudo swapon /mnt/swapfile
echo '/mnt/swapfile none swap sw 0 0' | sudo tee /etc/fstab
free -h
```

### Install Nix

https://nixos.org/manual/nix/stable/#chap-installation

https://github.com/nmattia/niv

https://input-output-hk.github.io/haskell.nix/tutorials/getting-started

```
# Single user install
sh <(curl -L https://nixos.org/nix/install)
source ~/.nix-profile/etc/profile.d/nix.sh

# Install niv
nix-env -i niv

# Configure Nix to use the binary cache from IOHK
sudo mkdir /etc/nix
cat << EOF | sudo tee /etc/nix/nix.conf
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
substituters = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org
EOF
```

### Install Docker

https://docs.docker.com/engine/install/debian

```
sudo apt-get install -y \
    apt-transport-https \
    ca-certificates \
    gnupg \
    lsb-release

curl -fsSL https://download.docker.com/linux/debian/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg

echo \
  "deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/debian \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

sudo apt-get update \
  && sudo apt-get install -y docker-ce docker-ce-cli containerd.io
  && sudo systemctl status docker

sudo usermod -aG docker $USER

# Verify docker access after restart
docker ps

# Install Docker Compose
sudo apt install -y python3-pip libffi-dev \
  && sudo pip3 install docker-compose
```

### Install Cabal + GHC

https://www.haskell.org/ghcup

```
sudo apt-get install -y build-essential libffi-dev libffi6 libgmp-dev \
  libgmp10 libncurses-dev libncurses5 libtinfo5

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
