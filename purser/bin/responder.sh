#! /bin/sh

#export LD_LIBRARY_PATH=../lib

NAME=responderd
DAEMON=./$NAME
DIR=`pwd`

CONFIGFILE=$DIR/../config/$NAME.conf
PIDFILE=$DIR/../log/$NAME.pid


#
# Function that starts the daemon/service
#
case "$1" in
  start)
	$DAEMON --configfile $CONFIGFILE --pidfile $PIDFILE
	;;
  stop)
	PID=`cat $PIDFILE 2>/dev/null`
        echo "Killing daemon: PID $PID"
	kill $PID
	;;
  restart)
	$0 stop
	$0 start
	;;

  force-stop)
        echo "Killing all: $NAME"
	killall -9 $NAME
	;;
  *)
	echo "Usage: $0 {start|stop|restart|force-stop}"
	exit 1
	;;
esac

