#! /bin/sh

export LD_LIBRARY_PATH=../lib

NAME=sender
PROG=/home/hukka/devel/purser/bin/$NAME
PORT=1233

$PROG --port $PORT


