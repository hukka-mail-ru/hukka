pwd

cd bin

echo "--------- receiver ------------"

./receiver.sh restart

echo "--------- responder ------------"

./responder.sh restart

echo "--------- send ------------"

./send.sh --phone 777 --text "Hello world!"
cat ../log/sender.log

echo "--------- responder log ------------"

cat ../log/responderd.log
