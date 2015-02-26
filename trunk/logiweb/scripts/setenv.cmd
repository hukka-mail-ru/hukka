@echo off
rem     Copyright (c) 1995 Novell, Inc.
rem       All Rights Reserved

rem     THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF
rem     Novell, Inc.
rem     The copyright notice above does not evidence any
rem     actual or intended publication of such source code.

rem     Copyright (c) 1995 Novell, Inc.
rem     All rights reserved
rem     ident	"@(#)apps:simpapp/setenv.cmd	60.1"      


rem set WSNADDR=<address of the server; only if this is a workstation client>

@echo on
set TUXDIR=e:\Tuxedo
set APPDIR=e:\amest_dev\servers
set ORACLE_HOME=E:\oracle\product\10.2.0\client_1
set PATH=%TUXDIR%\bin;%APPDIR%;%ORACLE_HOME%\bin;%PATH%
set TUXCONFIG=e:\amest_dev\tuxconfig
set QMCONFIG=e:\amest_dev\queues
set BDMCONFIG=%APPDIR%\BDMCONFIG
rem set NLS_LANG=AMERICAN_AMERICA.WE8ISO8859P1
set NLS_LANG=AMERICAN_AMERICA.WE8MSWIN1252

set FLDTBLDIR=e:\amest_dev\Fml
set FIELDTBLS=fml.tbl
set FLDTBLDIR32=e:\amest_dev\Fml
set FIELDTBLS32=fml.tbl,jrep.f32
rem set TMNOTHREADS=yes
set TMNOTHREADS=no
rem set ULOGMILLISEC=y
rem set TMTRACE=atmi:/tpa?call/ulog:dye

