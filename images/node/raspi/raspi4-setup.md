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

HOSTIP=192.168.0.5

ssh pi@$HOSTIP mkdir .ssh
scp ~/.ssh/id_rsa.pub pi@$HOSTIP:.ssh/authorized_keys
```

Verify that you can login using your private key. Then do ...

```
# Disable password authentication
sudo sed -i "s/^#PasswordAuthentication yes$/PasswordAuthentication no/" /etc/ssh/sshd_config
sudo cat /etc/ssh/sshd_config | grep PasswordAuthentication
sudo systemctl restart sshd

# Change default password
PASSWORD=`echo "changeme" | sha256sum | cut -d ' ' -f 1`
PASSWORD=${PASSWORD:0:12}
echo $PASSWORD

passwd 

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
sudo apt-get update
sudo apt-get full-upgrade -y

# Enable memory accounting
# https://github.com/raspberrypi/Raspberry-Pi-OS-64bit/issues/124
CMDLINE=`cat /boot/cmdline.txt`
echo "$CMDLINE cgroup_memory=1 cgroup_enable=memory" | sudo tee /boot/cmdline.txt
```

### Swap setup

```
sudo dphys-swapfile swapoff
sudo sed -i "s/^CONF_SWAPSIZE=100$/CONF_SWAPSIZE=8192/" /etc/dphys-swapfile
sudo sed -i "s/^#CONF_MAXSWAP=2048$/CONF_MAXSWAP=8192/" /etc/dphys-swapfile
sudo cat /etc/dphys-swapfile | grep CONF
sudo dphys-swapfile setup
sudo dphys-swapfile swapon
free -h
```

### Install Docker

https://docs.docker.com/engine/install/debian/

```
sudo apt-get install -y apt-transport-https ca-certificates gnupg-agent software-properties-common

curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add -

sudo add-apt-repository \
   "deb [arch=arm64] https://download.docker.com/linux/debian \
   $(lsb_release -cs) \
   stable"

sudo apt-get update
sudo apt-get install -y docker-ce docker-ce-cli containerd.io

sudo systemctl enable docker
sudo systemctl start docker
sudo systemctl status docker

sudo usermod -aG docker $USER

# Verify docker access after restart
docker run --rm centos echo "Hello World"
```
