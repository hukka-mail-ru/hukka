<b>DLL build:</b>
<pre>
g++ -c -fPIC client.cpp -o client.o<br>
gcc -shared -Wl,-soname,libclient.so -o libclient.so client.o -lc -lstdc++<br>
<br>
<br>
Invocation code build:<br>
g++ -ldl -lclient -L. main.cpp<br>
<br>
export LD_LIBRARY_PATH=~/devel/hukka/ ... where is the DLL<br>
<br>
</pre>
<b>JAVA</b><br>
<pre>
javac HelloWorld.java<br>
java HelloWorld<br>
</pre>

<b>JAVA + JNI</b><br>
<pre>
http://java.sun.com/docs/books/jni/download/jni.pdf<br>
<br>
javac HelloWorld.java<br>
javah -jni HelloWorld (запускать из каталога bin/classes, указывать полное имя класса)<br>
<br>
DLL-ку на С++ собирать так:<br>
g++ -c -fPIC client.cpp -o client.o -I/usr/lib/jvm/java-6-sun-1.6.0.22/include -I/usr/lib/jvm/java-6-sun/include/linux<br>
gcc -shared -Wl,-soname,libclient.so -o libclient.so client.o -lc -lstdc++<br>
<br>
java -Djava.library.path=. HelloWorld<br>
<br>
<br>
</pre>



<b>JAVA + JNI + QT</b><br>


<pre>

qmake<br>
make<br>
<br>
</pre>

<b>JAVA + Android emulator </b><br>
<pre>
http://rhrn.ru/articles/show/18/Ustanovka_SDK_Android_i_HelloWorld_pod_linux_(ubuntu)/<br>
<br>
1) android create project --package com.android.helloandroid --activity HelloAndroid --target 1 --path HelloAndroid<br>
<br>
2) Заходим в директорию HelloAndroid и выполняем ~/distr/qt_android/ant-1.8.0/bin/ant debug<br>
3) Запускаем эмулятор командой android<br>
<br>
4) adb install -r bin/HelloAndroid-debug.apk<br>
<br>
</pre>

<b>JAVA  + Android emulator + JNI + NDK</b><br>


<pre>

1) Создать проект (см. JAVA + Android emulator)<br>
<br>
2) В каталоге проекта выполнить: android update project -p . -s<br>
<br>
3) В каталоге проекта создать каталог jni и скопировать туда<br>
- исходники СРР-библиотеки,<br>
- h-файл с декларацией JNI-метода<br>
- файл ~/distr/qt_android/android-ndk-r6/samples/hello-jni/jni/Android.mk<br>
<br>
4) В каталоге проекта выполнить: ~/distr/qt_android/android-ndk-r6/ndk-build<br>
(создастся so-файл, скомпиленный для ARM)<br>
<br>
5) ~/distr/qt_android/ant-1.8.0/bin/ant debug<br>
6) adb install -r bin/HelloAndroid-debug.apk<br>
<br>
</pre>

<b>
ПРОВЕРКА: ВКЛЮЧЕНА ЛИ DLL В APK-файл<br>
<br>
jar tf HelloJni.apk<br>
<br>
<br>
Unknown end tag for </b><br>
<br>
