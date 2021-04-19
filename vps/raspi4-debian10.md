### Install 64bit OS

https://downloads.raspberrypi.org/raspios_arm64/images

https://github.com/raspberrypi/Raspberry-Pi-OS-64bit/issues


### Boot with SSH

Copy an empty file called 'SSH' to the bootable SSD Card

Username: pi
Password: raspberry

```
# Enable SSH on boot
touch /Volumes/boot/SSH

HOSTIP=192.168.0.55

scp ~/.ssh/id_rsa.pub pi@$HOSTIP:.ssh/authorized_keys
```

Verify that you can login using your private key. Then do ...

```
# Disable password authentication
sudo sed -i "s/^#PasswordAuthentication yes$/PasswordAuthentication no/" /etc/ssh/sshd_config
sudo cat /etc/ssh/sshd_config | grep PasswordAuthentication
sudo systemctl restart sshd

# Set the locale
sudo sed -i "s/^en_GB.UTF-8 UTF-8$/# en_GB.UTF-8 UTF-8/" /etc/locale.gen
sudo sed -i "s/^# en_US.UTF-8 UTF-8$/en_US.UTF-8 UTF-8/" /etc/locale.gen
cat /etc/locale.gen | grep en_

sudo locale-gen en_US.UTF-8

cat << EOF >> $HOME/.profile
# Locale and Language
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=C
EOF
source .profile

# Update the system
sudo apt-get update \
  && sudo apt-get full-upgrade -y

# Change the default runlevel
# System | Boot | Console
sudo rm /etc/systemd/system/default.target
sudo ln -s /lib/systemd/system/multi-user.target /etc/systemd/system/default.target

# Setup System Time synchronization
# https://www.digitalocean.com/community/tutorials/how-to-set-up-time-synchronization-on-debian-10
# sudo apt-get remove -y ntp ntpdate
# sudo systemctl start systemd-timesyncd
# sudo systemctl status systemd-timesyncd

timedatectl
timedatectl list-timezones | grep Berlin
sudo timedatectl set-timezone Europe/Berlin

# Enable memory accounting
# https://github.com/raspberrypi/Raspberry-Pi-OS-64bit/issues/124
CMDLINE=`cat /boot/cmdline.txt`
echo "$CMDLINE cgroup_memory=1 cgroup_enable=memory" | sudo tee /boot/cmdline.txt

# Enable KVM Kernel Module
# Needed for runAsRoot in Nix dockerTools.buildImage
sudo apt install -y virt-manager libvirt0 qemu-system
sudo usermod -aG libvirt-qemu $(whoami)
sudo chmod 666 /dev/kvm

# Restart after boot cmd changes
sudo shutdown -r now
```

### Swap setup

```
sudo dphys-swapfile swapoff
sudo sed -i "s/^CONF_SWAPSIZE=100$/CONF_SWAPSIZE=4096/" /etc/dphys-swapfile
sudo sed -i "s/^#CONF_MAXSWAP=2048$/CONF_MAXSWAP=4096/" /etc/dphys-swapfile
sudo cat /etc/dphys-swapfile | grep CONF
sudo dphys-swapfile setup
sudo dphys-swapfile swapon
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
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
substituters = https://hydra.iohk.io https://cache.nixos.org
EOF
```

### Install Docker

https://docs.docker.com/engine/install/debian
https://withblue.ink/2020/06/24/docker-and-docker-compose-on-raspberry-pi-os.html

```
sudo apt-get install -y apt-transport-https ca-certificates gnupg-agent software-properties-common \
  && curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add - \
  && sudo add-apt-repository \
   "deb https://download.docker.com/linux/debian \
   $(lsb_release -cs) \
   stable"

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

### Meassure Temperature

```
vcgencmd measure_temp

temp=40.4'C
```

### Mount Data Disks

```
# List block devices
lsblk

# DISK00 ########################################################################

DISK00=/dev/sda
MOUNT00=/mnt/disks/data00

# Create new empty filesystem
# sudo mkfs.ext4 -m 0 -E lazy_itable_init=0,discard $DISK00

# Mount block storage
sudo mkdir -p $MOUNT00; sudo mount -o discard,defaults $DISK00 $MOUNT00

# Append to /etc/fstab
echo "" | sudo tee --append /etc/fstab
echo "$DISK00  $MOUNT00  ext4   defaults,noatime,nofail 0 0" | sudo tee --append /etc/fstab

# DISK01 ########################################################################

DISK01=/dev/sdb
MOUNT01=/mnt/disks/data01

# Create new empty filesystem
# sudo mkfs.ext4 -m 0 -E lazy_itable_init=0,discard $DISK01

# Mount block storage
sudo mkdir -p $MOUNT01; sudo mount -o discard,defaults $DISK01 $MOUNT01

# Append to /etc/fstab
echo "$DISK01  $MOUNT01  ext4   defaults,noatime,nofail 0 0" | sudo tee --append /etc/fstab

# DISK02 ########################################################################

FSTYPE=vfat
DISK02=/dev/sdc2
MOUNT02=/mnt/disks/vfat

# Mount block storage
sudo mkdir -p $MOUNT02; sudo mount -t $FSTYPE -o rw,uid=$(id -u),gid=$(id -g) $DISK02 $MOUNT02
```