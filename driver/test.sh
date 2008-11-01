# remove module

lines1=$(sudo wc -l /var/log/kern | awk "{print \$1}")

ls > /dev/hello

lines2=$(sudo wc -l /var/log/kern | awk "{print \$1}")
lines=`expr $lines2 - $lines1` || lines=0

echo ">>>>>>>>"
sudo tail -n $lines /var/log/kern
echo "<<<<<<<<"