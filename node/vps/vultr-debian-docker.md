## Install Docker

* Debian10

### Setup the core user

```
ssh root@vps

sed -i "s/#PasswordAuthentication yes$/PasswordAuthentication no/" /etc/ssh/sshd_config
cat /etc/ssh/sshd_config | grep PasswordAuthentication
systemctl restart sshd

cat << EOF >> $HOME/.profile
# Locale and Language
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=C
EOF
source .profile

# Update the system
apt-get update
apt-get full-upgrade -y

# Install Time Service
apt-get install -y chrony
systemctl start chronyd
systemctl status chronyd

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

### Swap setup to avoid running out of memory

```
sudo fallocate -l 4G /mnt/swapfile
sudo dd if=/dev/zero of=/mnt/swapfile bs=1024 count=4M
sudo mkswap /mnt/swapfile
sudo chmod 600 /mnt/swapfile
sudo swapon /mnt/swapfile
echo '/mnt/swapfile none swap sw 0 0' | sudo tee /etc/fstab
free -h
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

curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add -

sudo add-apt-repository \
   "deb https://download.docker.com/linux/debian \
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

