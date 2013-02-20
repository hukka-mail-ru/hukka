#! /bin/sh

#export LD_LIBRARY_PATH=../lib

NAME=sender
PORT=1233
DIR=`pwd`
LOG=$DIR/../log/$NAME.log

./$NAME --port $PORT --log $LOG


