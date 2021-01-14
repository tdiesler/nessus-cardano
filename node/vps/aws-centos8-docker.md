
## Initialize

* CentOS 8
* 20 GB SSD
* 2 CPU
* 4 GB 

```
ssh centos@vps

sudo yum update -y

# Install Time Service
sudo timedatectl status
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
MOUNT00=/mnt/disks/data00

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
