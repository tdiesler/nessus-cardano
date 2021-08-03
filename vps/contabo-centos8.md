
* CentOS 8

### Setup the core user

```
ssh root@vps

# Update the system
yum update -y
yum install -y tar git epel-release

# Check Time Service
timedatectl set-local-rtc 0
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

https://docs.docker.com/engine/install/centos

```
sudo yum install -y yum-utils \
  && sudo yum-config-manager --add-repo \
    https://download.docker.com/linux/centos/docker-ce.repo

sudo yum install -y docker-ce docker-ce-cli containerd.io \
  && sudo systemctl enable --now docker \
  && sudo systemctl start docker

sudo usermod -aG docker $USER

# Verify docker access after restart
docker ps

# Install Docker Compose
sudo curl -L "https://github.com/docker/compose/releases/download/1.29.2/docker-compose-Linux-x86_64" \
  -o /usr/local/bin/docker-compose
```
