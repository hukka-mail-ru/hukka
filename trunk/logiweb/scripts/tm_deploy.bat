@echo on

set SHARED="L:\backend\LW_SERVERAPP\Tuxedo Shared Files"
set SERVER=E:\amest_dev
set SCRIPTS="L:\backend\scripts"

copy %SCRIPTS%\* %SERVER%

call setenv
tmshutdown -y -w1
tmshutdown -y -w1
tmshutdown -y -w1
perl deploy.pl


copy %SHARED%\repositorybulk.txt %SERVER%
copy %SHARED%\fml.tbl %SERVER%\FML
copy %SHARED%\Compiled_FML\fml.tbl.h %SERVER%\FML


call tmboot -y

cd %SERVER%
bulk-ssadovni_pwd_gilliche.bat