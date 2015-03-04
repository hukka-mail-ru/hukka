@echo off
REM setlocal
REM user password: gilliche

java -cp "%TUXDIR%\udataobj\jolt\joltadmin.jar;%TUXDIR%\udataobj\jolt\jolt.jar" bea.jolt.admin.jbld -u egillich\ewes -p gilliche //10.0.2.15:29878 repositorybulk.txt
tmshutdown -s JREPSVR -s JSL
tmboot     -s JSL -s JREPSVR

