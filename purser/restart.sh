pwd

cd bin

echo "--------- receiver ------------"

./receiver.sh restart

echo "--------- responder ------------"

./responder.sh restart

echo "--------- send ------------"

./send.sh

echo "--------- responder log ------------"

cat ../log/responderd.log
