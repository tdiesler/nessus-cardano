
## Initialize

* Debian 9

```
ssh admin@vps

cat << EOF >> $HOME/.profile
# Locale and Language
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=C
EOF
source .profile

sudo apt-get update -y
sudo apt-get upgrade -y

# Check Time Service
timedatectl

NUSER=core
sudo useradd -G root -m $NUSER -s /bin/bash
sudo cp -r .ssh /home/$NUSER/
sudo chown -R $NUSER.$NUSER /home/$NUSER/.ssh

cat << EOF | sudo tee /etc/sudoers.d/user-privs-$NUSER
$NUSER ALL=(ALL:ALL) NOPASSWD: ALL
EOF
```

### Swap setup to avoid running out of memory

```
sudo fallocate -l 8G /mnt/swapfile
sudo dd if=/dev/zero of=/mnt/swapfile bs=1024 count=8M
sudo mkswap /mnt/swapfile
sudo chmod 600 /mnt/swapfile
sudo swapon /mnt/swapfile
echo '/mnt/swapfile none swap sw 0 0' >> /etc/fstab
free -h
```

### Install Docker

```
sudo yum install -y yum-utils

sudo yum-config-manager --add-repo \
    https://download.docker.com/linux/centos/docker-ce.repo

sudo yum install -y docker-ce docker-ce-cli containerd.io
sudo systemctl enable docker
sudo systemctl start docker

sudo usermod -aG docker $USER

docker ps
```

### Mount Data Disks

```
# List block devices
lsblk

# DISK00 ########################################################################

DISK00=/dev/nvme1n1
MOUNT00=/nix

# Create new empty filesystem:
# sudo mkfs.ext4 -m 0 -E lazy_itable_init=0,discard $DISK00

# Mount block storage
sudo mkdir -p $MOUNT00; sudo mount -o discard,defaults $DISK00 $MOUNT00

# Append to /etc/fstab
echo "" | sudo tee --append /etc/fstab
echo "$DISK00  $MOUNT00  ext4   defaults,noatime,nofail 0 0" | sudo tee --append /etc/fstab

# DISK01 ########################################################################

DISK01=/dev/nvme2n1
MOUNT01=/mnt/disks/data01

# Create new empty filesystem:
# sudo mkfs.ext4 -m 0 -E lazy_itable_init=0,discard $DISK01

# Mount block storage
sudo mkdir -p $MOUNT01; sudo mount -o discard,defaults $DISK01 $MOUNT01

# Append to /etc/fstab
echo "$DISK01  $MOUNT01  ext4   defaults,noatime,nofail 0 0" | sudo tee --append /etc/fstab
```
