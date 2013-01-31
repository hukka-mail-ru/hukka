#! /bin/sh

NAME=example_daemon
DAEMON=/home/hukka/devel/purser/daemon/Debug/$NAME
#PIDFILE=/var/run/$NAME.pid
PIDFILE=/home/hukka/devel/purser/daemon/$NAME.pid


#
# Function that starts the daemon/service
#
case "$1" in
  start)
	$DAEMON --pidfile $PIDFILE
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
	PID=`cat $PIDFILE 2>/dev/null`
        echo "Killing daemon: PID $PID"
	kill -9 $PID
	;;
  *)
	echo "Usage: $0 {start|stop|restart|force-stop}"
	exit 1
	;;
esac

