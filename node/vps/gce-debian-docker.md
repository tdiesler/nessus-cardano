
## Initialize

* Debian10 (Buster)

* e2-medium (2 vCPUs, 4 GB memory) 

```
ssh core@vps

cat << EOF >> $HOME/.profile

# Locale and Language
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=C
EOF
source .profile

sudo apt-get update -y
sudo apt-get upgrade -y

# Install Time Service
sudo apt-get install -y ntp ntpdate
sudo systemctl enable ntp
sudo systemctl start ntp
```

### Install Docker

https://docs.docker.com/engine/install/debian

```
sudo apt-get install -y apt-transport-https ca-certificates gnupg-agent software-properties-common

curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add -

sudo add-apt-repository \
   "deb https://download.docker.com/linux/debian \
   $(lsb_release -cs) \
   stable"
      
sudo apt update
sudo apt-get install -y docker-ce docker-ce-cli containerd.io

sudo usermod -aG docker $USER

# Verify docker access after restart
docker run --rm debian echo "Hello World"
```

### GCE Monitoring Agent

```
curl -sSO https://dl.google.com/cloudagents/add-monitoring-agent-repo.sh
sudo bash add-monitoring-agent-repo.sh
sudo rm add-monitoring-agent-repo.sh
sudo apt-get update
sudo apt-get install -y stackdriver-agent
sudo service stackdriver-agent start
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

### Mount Data Disks

```
# List block devices
lsblk

# DISK00 ########################################################################

DISK00=/dev/sdb
MOUNT00=/mnt/disks/data00

# Create new empty filesystem:
# sudo mkfs.ext4 -m 0 -E lazy_itable_init=0,discard $DISK00

# Mount block storage
sudo mkdir -p $MOUNT00; sudo mount -o discard,defaults $DISK00 $MOUNT00

# Append to /etc/fstab
echo "" | sudo tee --append /etc/fstab
echo "$DISK00  $MOUNT00  ext4   defaults,noatime,nofail 0 0" | sudo tee --append /etc/fstab

# DISK01 ########################################################################

DISK01=/dev/sdc
MOUNT01=/mnt/disks/data01

# Create new empty filesystem:
# sudo mkfs.ext4 -m 0 -E lazy_itable_init=0,discard $DISK01

# Mount block storage
sudo mkdir -p $MOUNT01; sudo mount -o discard,defaults $DISK01 $MOUNT01

# Append to /etc/fstab
echo "$DISK01  $MOUNT01  ext4   defaults,noatime,nofail 0 0" | sudo tee --append /etc/fstab
```
