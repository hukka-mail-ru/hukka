#! /bin/sh

NAME=sender
PROG=/home/hukka/devel/purser/bin/$NAME
LOGFILE=/home/hukka/devel/purser/log/$NAME.log
PORT=1233


$PROG --logfile $LOGFILE --port $PORT

