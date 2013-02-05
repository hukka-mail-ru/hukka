#! /bin/sh

NAME=sender
PROG=/home/hukka/devel/purser/bin/$NAME
PIDFILE=/home/hukka/devel/purser/log/$NAME.pid
LOGFILE=/home/hukka/devel/purser/log/$NAME.log


$PROG --pidfile $PIDFILE --logfile $LOGFILE

