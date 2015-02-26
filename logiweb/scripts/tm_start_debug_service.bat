@echo on
call tmshutdown -y -w3 -s %1
call tmboot -n -d 1 -s %1
