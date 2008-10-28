major=70
minor=0

# remove module
echo "Remove 'hello' module..."
sudo /sbin/rmmod hello 
echo "OK"

# insert module
sudo /sbin/insmod ./src/hello.ko major=$major minor=$minor || exit 1

# remove stale node, create a new one
sudo rm -f /dev/hello 
sudo mknod /dev/hello c $major $minor || exit 1
echo "mknod /dev/hello c $major $minor"

# change permissions
group="staff"
grep -q '^staff:' /etc/group || group="wheel"
sudo chgrp $group /dev/hello
sudo chmod 666 /dev/hello

# show logging
DATE=`date`
TIME=`expr substr "$DATE" 5 16`

sleep 1
echo ">>>>>>>>"
sudo tail /var/log/kern
echo "<<<<<<<<"
echo $TIME