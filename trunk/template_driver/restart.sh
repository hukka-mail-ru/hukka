



lines1=$(sudo wc -l /var/log/kern | awk "{print \$1}")

module=timings

for i in 1
do
# remove module
echo "Removing modules..."
sudo /sbin/rmmod $module 
echo "OK"

# insert module
sudo /sbin/insmod ./src/$module.ko major=0 minor=0 || break

# get version
major=$(awk "\$2==\"$module\" {print \$1}" /proc/devices)
minor=0

# remove stale node, create a new one
sudo rm -f /dev/$module 

echo "mknod /dev/$module c $major $minor"
sudo mknod /dev/$module c $major $minor || break


# change permissions
group="staff"
grep -q '^staff:' /etc/group || group="wheel"
sudo chgrp $group /dev/$module
sudo chmod 666 /dev/$module


sleep 1
done


# END: show logging
lines2=$(sudo wc -l /var/log/kern | awk "{print \$1}")

lines=`expr $lines2 - $lines1` || lines=0

echo ">>>>>>>>"
sudo tail -n $lines /var/log/kern
echo "<<<<<<<<"

