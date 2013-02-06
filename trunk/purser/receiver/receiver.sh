#! /bin/sh

NAME=receiver
DAEMON=/home/hukka/devel/purser/receiver/Debug/$NAME
#PIDFILE=/var/run/$NAME.pid
PIDFILE=/home/hukka/devel/purser/log/$NAME.pid
LOGFILE=/home/hukka/devel/purser/log/$NAME.log
PORT=1233


#
# Function that starts the daemon/service
#
case "$1" in
  start)
	$DAEMON --pidfile $PIDFILE --logfile $LOGFILE --port $PORT
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

