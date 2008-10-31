module=hello

# remove module
echo "Remove '$module' module..."
sudo /sbin/rmmod $module 
echo "OK"

# insert module
sudo /sbin/insmod ./src/$module.ko || exit 1

# get version
major=$(awk "\$2==\"$module\" {print \$1}" /proc/devices)
minor=0


# remove stale node, create a new one
sudo rm -f /dev/$module 
sudo mknod /dev/$module c $major $minor || exit 1
echo "mknod /dev/$module c $major $minor"

# change permissions
group="staff"
grep -q '^staff:' /etc/group || group="wheel"
sudo chgrp $group /dev/$module
sudo chmod 666 /dev/$module

# show logging
DATE=`date`
TIME=`expr substr "$DATE" 5 16`

sleep 1
echo ">>>>>>>>"
sudo tail /var/log/kern
echo "<<<<<<<<"
echo $TIME