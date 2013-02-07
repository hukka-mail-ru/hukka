#! /bin/sh

NAME=responderd
DAEMON=/home/hukka/devel/purser/bin/$NAME

CONFIGFILE=/home/hukka/devel/purser/bin/$NAME.conf
PIDFILE=/home/hukka/devel/purser/log/$NAME.pid


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

