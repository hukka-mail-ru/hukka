# remove module

#lines1=$(sudo wc -l /var/log/kern | awk "{print \$1}")

for ((i=1; i <= 100 ; i++))
do
echo ">>>>>>>> $i"
ls -l > /dev/hello
cat /dev/hello
sleep 0.1
done

#lines2=$(sudo wc -l /var/log/kern | awk "{print \$1}")
#lines=`expr $lines2 - $lines1` || lines=0

#echo ">>>>>>>>"
#sudo tail -n $lines /var/log/kern
#echo "<<<<<<<<"