@echo off

call setenv

echo stopping...
tmshutdown -y -w 20

del tuxconfig.old
move tuxconfig tuxconfig.old

echo building new tuxconfig...
echo application password: amest
tmloadcf -y ubbconfig

pause

echo starting...
tmboot -y
