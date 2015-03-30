@echo on

call tmshutdown -y -w3 -s %1

copy L:\backend\LW_SERVERAPP\%1\Release\%1.exe E:\amest_dev\servers

call tmboot -y -s %1